use bevy::app::App;
use bevy::color::palettes::css::*;
use bevy::prelude::*;
use rand;
use rand::Rng;
use std::time::Duration;

// Game balance constants
const SHIP_HEALTH: f32 = 100.0;
const SHIP_RADIUS: f32 = 15.0;
const SHIP_ACCELERATION: f32 = 1000.0;
const SHIP_MAX_SPEED: f32 = 300.0;
const SHIP_FRICTION: f32 = 0.98;
const SHIP_RECOIL_FORCE: f32 = 5.0;

const BUBBLE_MAX_SUPPLY: f32 = 100.0;
const BUBBLE_COST: f32 = 1.0;
const BUBBLE_REGEN_RATE: f32 = 50.0; // Per second
const BUBBLE_MIN_SIZE: f32 = 5.0;
const BUBBLE_MAX_SIZE: f32 = 15.0;
const BUBBLE_MIN_SPEED: f32 = 100.0;
const BUBBLE_MAX_SPEED: f32 = 200.0;
const BUBBLE_MIN_LIFETIME: f32 = 1.0;
const BUBBLE_MAX_LIFETIME: f32 = 2.0;
const BUBBLE_SPREAD_ANGLE: f32 = 0.3;
const BUBBLE_DAMAGE: f32 = 10.0;

const ENEMY_HEALTH: f32 = 100.0;
const ENEMY_RADIUS: f32 = 20.0;
const ENEMY_MIN_SPEED: f32 = -50.0;
const ENEMY_MAX_SPEED: f32 = 50.0;
const ENEMY_COLLISION_DAMAGE: f32 = 20.0;
const ENEMY_COLLISION_FORCE: f32 = 400.0;

const BORDER_WIDTH: f32 = 50.0;
const BORDER_DAMAGE: f32 = 10.0;
const BORDER_BOUNCE_FORCE: f32 = 500.0;

const EXPLOSION_PARTICLES: u32 = 30;
const EXPLOSION_MIN_SPEED: f32 = 100.0;
const EXPLOSION_MAX_SPEED: f32 = 300.0;
const EXPLOSION_MIN_SIZE: f32 = 2.0;
const EXPLOSION_MAX_SIZE: f32 = 8.0;
const EXPLOSION_LIFETIME: f32 = 1.0;
const EXPLOSION_DRAG: f32 = 0.98;

// Add growth constants
const ENEMY_GROWTH_TIME: f32 = 1.0;
const ENEMY_MIN_SCALE: f32 = 0.1;

// Add at the top with other constants
const ENEMY_SPAWN_MARGIN: f32 = 100.0; // Keep enemies away from borders on spawn

// Add velocity scaling constants
const ENEMY_SPEED_SCALE_TIME: f32 = 60.0; // Time in seconds to reach max speed
const ENEMY_MAX_SPEED_MULTIPLIER: f32 = 3.0; // Maximum speed multiplier

// Add explosion type enum
#[derive(Component, Clone, Copy)]
enum ExplosionType {
    Ship,
    Enemy { color: Srgba },
}

// Update enemy explosion constants
const ENEMY_EXPLOSION_PARTICLES: u32 = 15;
const ENEMY_EXPLOSION_MIN_SPEED: f32 = 50.0;
const ENEMY_EXPLOSION_MAX_SPEED: f32 = 150.0;
const ENEMY_EXPLOSION_MIN_SIZE: f32 = 1.0;
const ENEMY_EXPLOSION_MAX_SIZE: f32 = 4.0;
const ENEMY_EXPLOSION_LIFETIME: f32 = 0.5;

#[derive(PartialEq)]
enum AimMode {
    Mouse,
    Keyboard,
}

#[derive(States, Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
enum GameState {
    #[default]
    Starting,
    Playing,
    Dying,
    GameOver,
}

#[derive(Resource, Deref, DerefMut)]
struct StartingTimer(Timer);

// Add spawn timer resource
#[derive(Resource)]
struct EnemySpawnTimer {
    timer: Timer,
    elapsed_time: f32,
    min_spawn_time: f32,
}

impl Default for EnemySpawnTimer {
    fn default() -> Self {
        Self {
            timer: Timer::from_seconds(3.0, TimerMode::Repeating),
            elapsed_time: 0.0,
            min_spawn_time: 0.5, // Fastest spawn rate
        }
    }
}

// First, create a helper function for state conditions
fn is_playing_or_dying(state: Res<State<GameState>>) -> bool {
    matches!(state.get(), GameState::Playing | GameState::Dying)
}

// Add bubble supply config
const MAX_BUBBLE_SUPPLY: f32 = 100.0;

// Add death timer resource
#[derive(Resource, Deref, DerefMut)]
struct DeathTimer(Timer);

// Add drip timer resource
#[derive(Resource, Deref, DerefMut)]
struct DripTimer(Timer);

// Add exit system
fn handle_exit(keyboard: Res<ButtonInput<KeyCode>>, mut app_exit_events: EventWriter<AppExit>) {
    if keyboard.just_pressed(KeyCode::Escape) {
        app_exit_events.send(AppExit::default());
    }
}

// Add marker component for game over UI
#[derive(Component)]
struct GameOverUI;

// Add audio resource
#[derive(Resource, Default)]
struct GameAudio {
    bubble_shoot: Handle<AudioSource>,
    pop: Handle<AudioSource>,
    bounce: Handle<AudioSource>,
    drip: Handle<AudioSource>,
}

// Add shooting state component
#[derive(Component)]
struct ShootingState {
    is_shooting: bool,
    sound_timer: Timer,
}

impl Default for ShootingState {
    fn default() -> Self {
        Self {
            is_shooting: false,
            sound_timer: Timer::from_seconds(0.1, TimerMode::Repeating),
        }
    }
}

// Add bubble shot event
#[derive(Event)]
struct BubbleShot;

// Add speed tracking component
#[derive(Component)]
struct EnemySpeed {
    current_speed: f32,
    max_speed: f32,
    acceleration: f32,
}

impl Default for EnemySpeed {
    fn default() -> Self {
        Self {
            current_speed: ENEMY_MIN_SPEED,
            max_speed: ENEMY_MAX_SPEED,
            acceleration: 50.0, // Speed increase per bounce
        }
    }
}

// Add enemy destroyed event
#[derive(Event)]
struct EnemyDestroyed;

// Add ship bounce event
#[derive(Event)]
struct ShipBounced;

// Add enemy hit event
#[derive(Event)]
struct EnemyHit;

// Update score resource
#[derive(Resource, Default)]
struct Score {
    value: f32,
    time_points: f32, // Points from surviving
    kill_points: f32, // Points from destroying enemies
}

const POINTS_PER_SECOND: u32 = 10;
const POINTS_PER_ENEMY: u32 = 100;

// Update score systems
fn update_score(mut score: ResMut<Score>, time: Res<Time>) {
    let points = time.delta_secs() * POINTS_PER_SECOND as f32;
    score.time_points += points;
    score.value = score.time_points + score.kill_points;
}

fn handle_enemy_destroyed(
    mut score: ResMut<Score>,
    mut enemy_destroyed: EventReader<EnemyDestroyed>,
) {
    for _ in enemy_destroyed.read() {
        score.kill_points += POINTS_PER_ENEMY as f32;
        score.value = score.time_points + score.kill_points;
    }
}

// Add score display
#[derive(Component)]
struct ScoreText;

fn spawn_score_ui(mut commands: Commands) {
    commands.spawn((
        Node {
            position_type: PositionType::Absolute,
            left: Val::Px(10.0),
            top: Val::Px(10.0),
            ..default()
        },
        ScoreText,
        Text::new("Score: 0"),
    ));
}

fn update_score_display(score: Res<Score>, mut query: Query<&mut Text, With<ScoreText>>) {
    if let Ok(mut text) = query.get_single_mut() {
        text.0 = format!("Score: {}", score.value as u32);
    }
}

pub struct GamePlugin;

impl Plugin for GamePlugin {
    fn build(&self, app: &mut App) {
        app.init_state::<GameState>()
            .insert_resource(StartingTimer(Timer::from_seconds(1.0, TimerMode::Once)))
            .insert_resource(EnemySpawnTimer::default())
            .insert_resource(DeathTimer(Timer::from_seconds(1.0, TimerMode::Once)))
            .insert_resource(DripTimer(Timer::from_seconds(0.05, TimerMode::Repeating)))
            .add_event::<BubbleShot>()
            .add_event::<EnemyDestroyed>()
            .add_event::<ShipBounced>()
            .add_event::<EnemyHit>()
            .insert_resource(Score::default())
            .add_systems(Startup, setup)
            .insert_resource(ClearColor(Color::BLACK))
            .insert_resource(Mouse::default())
            .add_systems(
                Update,
                (
                    // Update systems - only run during Playing
                    calculate_mouse_position,
                    spawn_bubble,
                    move_bubbles,
                    despawn_bubbles,
                    move_ship,
                    update_aim_control,
                    spawn_enemies,
                    check_bubble_enemy_collision,
                    update_bubble_lifetime,
                    handle_ship_border,
                    check_game_over,
                    handle_ship_enemy_collision,
                    regenerate_bubble_supply,
                    update_enemy_growth,
                    handle_enemy_border,
                    update_shooting_state,
                    handle_bubble_sound,
                    handle_enemy_death_sound,
                    handle_ship_bounce_sound,
                    handle_enemy_hit_sound,
                )
                    .run_if(in_state(GameState::Playing)),
            )
            .add_systems(
                Update,
                (update_score, handle_enemy_destroyed).run_if(in_state(GameState::Playing)),
            )
            .add_systems(
                Update,
                (
                    // Draw systems - run during both Playing and Dying
                    draw_ship,
                    draw_bubbles,
                    draw_enemies,
                    update_explosion,
                    draw_explosion,
                    update_score_display,
                )
                    .run_if(is_playing_or_dying),
            )
            .add_systems(OnEnter(GameState::GameOver), spawn_game_over_ui)
            .add_systems(OnExit(GameState::Dying), cleanup_gameplay)
            .add_systems(
                OnEnter(GameState::Starting),
                (setup_game_round, spawn_get_ready_text),
            )
            .add_systems(OnExit(GameState::Starting), cleanup_get_ready_text)
            .add_systems(
                Update,
                handle_starting_state.run_if(in_state(GameState::Starting)),
            )
            .add_systems(
                Update,
                handle_replay_button.run_if(in_state(GameState::GameOver)),
            )
            .add_systems(OnEnter(GameState::Dying), spawn_ship_explosion)
            .add_systems(
                Update,
                handle_death_timer.run_if(in_state(GameState::Dying)),
            )
            .add_systems(Update, handle_exit)
            .add_systems(OnExit(GameState::GameOver), cleanup_game_over_ui)
            .add_systems(Startup, spawn_score_ui);
    }
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    // Load and store audio assets
    commands.insert_resource(GameAudio {
        bubble_shoot: asset_server.load("audio/sound_effects/bubble.ogg"),
        pop: asset_server.load("audio/sound_effects/pop.ogg"),
        bounce: asset_server.load("audio/sound_effects/bounce.ogg"),
        drip: asset_server.load("audio/sound_effects/drip.ogg"),
    });
}

#[derive(Component, Default)]
struct Velocity(Vec2);

#[derive(Component)]
struct Bubble {
    color: Color,
    size: f32,
    lifetime: Timer,
}

#[derive(Component)]
#[require(Transform, Velocity)]
struct Ship {
    health: f32,
    bubble_supply: f32, // 0.0 to 100.0
}

#[derive(Component)]
struct Enemy {
    health: f32,
    variant: EnemyVariant,
    color: Color,
}

#[derive(Component)]
enum EnemyVariant {
    Floater,
    Seeker,
    // Add more variants as we implement them
}

fn random_pastel_color() -> Color {
    let mut rng = rand::thread_rng();
    Color::hsl(
        rng.gen_range(0.0..360.0), // Random hue
        0.7,                       // High saturation
        0.8,                       // High lightness for pastel
    )
}

// Update AimControl component
#[derive(Component)]
struct AimControl {
    mode: AimMode,
    angle: f32, // Angle in radians
}

impl Default for AimControl {
    fn default() -> Self {
        Self {
            mode: AimMode::Mouse,
            angle: 0.0, // Start aiming right
        }
    }
}

// Update aim control system
fn update_aim_control(
    mut query: Query<(&Transform, &mut AimControl)>,
    keyboard: Res<ButtonInput<KeyCode>>,
    mouse: Res<Mouse>,
    mouse_button: Res<ButtonInput<MouseButton>>,
) {
    if let Ok((transform, mut aim)) = query.get_single_mut() {
        let pos = transform.translation.truncate();

        // Check for mode switches
        if mouse_button.pressed(MouseButton::Left) {
            aim.mode = AimMode::Mouse;
        } else if keyboard.any_pressed([KeyCode::KeyW, KeyCode::KeyA, KeyCode::KeyS, KeyCode::KeyD])
        {
            aim.mode = AimMode::Keyboard;
        }

        match aim.mode {
            AimMode::Mouse => {
                let to_mouse = mouse.position - pos;
                aim.angle = Vec2::new(to_mouse.x, -to_mouse.y).angle_to(Vec2::X);
            }
            AimMode::Keyboard => {
                let mut direction = Vec2::ZERO;
                if keyboard.pressed(KeyCode::KeyW) {
                    direction.y += 1.0;
                }
                if keyboard.pressed(KeyCode::KeyS) {
                    direction.y -= 1.0;
                }
                if keyboard.pressed(KeyCode::KeyA) {
                    direction.x += 1.0;
                }
                if keyboard.pressed(KeyCode::KeyD) {
                    direction.x -= 1.0;
                }

                if direction != Vec2::ZERO {
                    aim.angle = direction.normalize().angle_to(Vec2::X);
                }
            }
        }
    }
}

// Update shooting state
fn update_shooting_state(
    mut query: Query<(&AimControl, &mut ShootingState)>,
    keyboard: Res<ButtonInput<KeyCode>>,
    mouse_button: Res<ButtonInput<MouseButton>>,
) {
    if let Ok((aim, mut shooting)) = query.get_single_mut() {
        shooting.is_shooting = match aim.mode {
            AimMode::Mouse => mouse_button.pressed(MouseButton::Left),
            AimMode::Keyboard => {
                keyboard.any_pressed([KeyCode::KeyW, KeyCode::KeyA, KeyCode::KeyS, KeyCode::KeyD])
            }
        };
    }
}

// Update bubble spawning to send event
fn spawn_bubble(
    mut commands: Commands,
    mut ship_query: Query<(
        &Transform,
        &mut Ship,
        &mut Velocity,
        &ShootingState,
        &AimControl,
    )>,
    mut bubble_shot: EventWriter<BubbleShot>,
) {
    if let Ok((ship_transform, mut ship, mut ship_vel, shooting, aim)) = ship_query.get_single_mut()
    {
        if shooting.is_shooting && ship.bubble_supply >= BUBBLE_COST {
            ship.bubble_supply -= BUBBLE_COST;
            let ship_pos = ship_transform.translation.truncate();
            let mut rng = rand::thread_rng();

            // Use current aim angle for direction
            let direction = Vec2::from_angle(aim.angle);
            let random_angle = rng.gen_range(-BUBBLE_SPREAD_ANGLE..BUBBLE_SPREAD_ANGLE);
            let rotated_direction = Vec2::new(
                direction.x * random_angle.cos() - direction.y * random_angle.sin(),
                direction.x * random_angle.sin() + direction.y * random_angle.cos(),
            );
            let speed = rng.gen_range(BUBBLE_MIN_SPEED..BUBBLE_MAX_SPEED);

            // Apply recoil to ship
            ship_vel.0 -= rotated_direction * SHIP_RECOIL_FORCE;

            commands.spawn((
                Bubble {
                    color: random_pastel_color(),
                    size: rng.gen_range(BUBBLE_MIN_SIZE..BUBBLE_MAX_SIZE),
                    lifetime: Timer::from_seconds(
                        rng.gen_range(BUBBLE_MIN_LIFETIME..BUBBLE_MAX_LIFETIME),
                        TimerMode::Once,
                    ),
                },
                Transform::from_xyz(ship_pos.x, ship_pos.y, 0.0),
                Velocity(rotated_direction * speed),
                GameplayObject,
            ));

            bubble_shot.send(BubbleShot);
        }
    }
}

fn move_bubbles(mut query: Query<(&mut Transform, &Velocity)>, time: Res<Time>) {
    for (mut transform, velocity) in &mut query {
        transform.translation += velocity.0.extend(0.0) * time.delta_secs()
    }
}

#[derive(Resource, Debug, Default)]
struct Mouse {
    position: Vec2,
}

fn calculate_mouse_position(
    camera_query: Query<(&GlobalTransform, &Camera)>,
    window_query: Query<&Window>,
    mut mouse: ResMut<Mouse>,
) {
    let (camera_transform, camera) = camera_query.single();
    let window = window_query.single();

    let position = window
        .cursor_position()
        .and_then(|cursor| camera.viewport_to_world_2d(camera_transform, cursor).ok())
        .unwrap_or_default();

    mouse.position = position;
}

fn draw_bubbles(mut gizmos: Gizmos, query: Query<(&Transform, &Bubble)>) {
    for (transform, bubble) in &query {
        let pos = transform.translation.truncate();
        let radius = bubble.size;

        // Outer glow
        gizmos.circle_2d(pos, radius + 2.0, bubble.color.with_alpha(0.2));

        // Main bubble outline
        gizmos.circle_2d(pos, radius, bubble.color.with_alpha(0.8));

        // Inner highlight
        gizmos.circle_2d(
            pos + Vec2::new(-radius * 0.2, radius * 0.2),
            radius * 0.4,
            Color::WHITE.with_alpha(0.3),
        );

        // Shine detail
        gizmos.circle_2d(
            pos + Vec2::new(-radius * 0.1, radius * 0.1),
            radius * 0.2,
            Color::WHITE.with_alpha(0.5),
        );
    }
}

fn despawn_bubbles(
    mut commands: Commands,
    query: Query<(Entity, &Transform), With<Bubble>>,
    window_query: Query<&Window>,
) {
    let window = window_query.single();
    let half_width = window.width() / 2.0;
    let half_height = window.height() / 2.0;

    for (entity, transform) in &query {
        let pos = transform.translation;
        if pos.x < -half_width || pos.x > half_width || pos.y < -half_height || pos.y > half_height
        {
            commands.entity(entity).despawn();
        }
    }
}

fn move_ship(
    mut query: Query<(&mut Transform, &mut Velocity), With<Ship>>,
    time: Res<Time>,
    window_query: Query<&Window>,
) {
    if let Ok((mut transform, mut velocity)) = query.get_single_mut() {
        let window = window_query.single();
        let half_width = window.width() / 2.0;
        let half_height = window.height() / 2.0;

        let mut acceleration = Vec2::ZERO;
        let friction = SHIP_FRICTION;

        let dt = time.delta_secs();

        if acceleration != Vec2::ZERO {
            acceleration = acceleration.normalize() * SHIP_ACCELERATION * dt;
            velocity.0 += acceleration;
        }

        // Apply friction
        velocity.0 *= friction;

        // Clamp maximum speed
        if velocity.0.length() > SHIP_MAX_SPEED {
            velocity.0 = velocity.0.normalize() * SHIP_MAX_SPEED;
        }

        transform.translation += velocity.0.extend(0.0) * dt;

        // Wrap position around screen edges
        if transform.translation.x > half_width {
            transform.translation.x = -half_width;
        } else if transform.translation.x < -half_width {
            transform.translation.x = half_width;
        }

        if transform.translation.y > half_height {
            transform.translation.y = -half_height;
        } else if transform.translation.y < -half_height {
            transform.translation.y = half_height;
        }
    }
}

fn draw_ship(
    mut gizmos: Gizmos,
    query: Query<(&Transform, &Ship, &AimControl)>,
    window_query: Query<&Window>,
) {
    let window = window_query.single();
    let border_width = BORDER_WIDTH;

    // Draw danger border
    gizmos.rect_2d(
        Vec2::ZERO,
        Vec2::new(
            window.width() - border_width,
            window.height() - border_width,
        ),
        Color::srgba(1.0, 0.0, 0.0, 0.2),
    );

    if let Ok((transform, ship, aim)) = query.get_single() {
        let pos = transform.translation.truncate();

        // Calculate ship colors based on health
        let health_factor = (ship.health / 100.0).clamp(0.0, 1.0);
        let ship_color = Color::srgb(1.0, health_factor, health_factor);

        // Draw outer ship circle
        gizmos.circle_2d(pos, SHIP_RADIUS, ship_color);

        // Draw inner bubble supply circle
        let bubble_radius = 10.0 * (ship.bubble_supply / MAX_BUBBLE_SUPPLY);
        let bubble_color = Color::srgb(0.3, 0.8, 1.0);
        gizmos.circle_2d(pos, bubble_radius, bubble_color);

        // Draw aim line using current aim angle
        let aim_direction = Vec2::from_angle(aim.angle);
        let rect_length = 20.0;
        let rect_center = pos + aim_direction * 15.0;

        // Draw aim line with different styles based on mode
        match aim.mode {
            AimMode::Mouse => {
                gizmos.line_2d(
                    rect_center - aim_direction * rect_length / 2.0,
                    rect_center + aim_direction * rect_length / 2.0,
                    ship_color,
                );
            }
            AimMode::Keyboard => {
                // Draw a dashed/animated line for keyboard mode
                let segments = 5;
                let segment_length = rect_length / segments as f32;
                let time = (aim.angle / std::f32::consts::PI * 2.0).sin() * 0.5 + 0.5;

                for i in 0..segments {
                    let start = rect_center - aim_direction * rect_length / 2.0
                        + aim_direction * i as f32 * segment_length;
                    let end = start + aim_direction * segment_length * 0.7;
                    let alpha = (i as f32 / segments as f32 + time) % 1.0;
                    gizmos.line_2d(start, end, ship_color.with_alpha(alpha));
                }
            }
        }
    }
}

// Add helper function to calculate current speed multiplier
fn get_enemy_speed_multiplier(elapsed_time: f32) -> f32 {
    1.0 + (elapsed_time / ENEMY_SPEED_SCALE_TIME * (ENEMY_MAX_SPEED_MULTIPLIER - 1.0))
        .min(ENEMY_MAX_SPEED_MULTIPLIER - 1.0)
}

// Update spawn_enemies to use speed scaling
fn spawn_enemies(
    mut commands: Commands,
    time: Res<Time>,
    window_query: Query<&Window>,
    mut spawn_timer: ResMut<EnemySpawnTimer>,
) {
    spawn_timer.elapsed_time += time.delta_secs();

    // Gradually decrease spawn time (3.0 -> 0.5 seconds over 60 seconds)
    let current_spawn_time =
        (3.0 - (spawn_timer.elapsed_time / 60.0) * 2.5).max(spawn_timer.min_spawn_time);
    spawn_timer
        .timer
        .set_duration(Duration::from_secs_f32(current_spawn_time));

    // Get current speed multiplier
    let speed_multiplier = get_enemy_speed_multiplier(spawn_timer.elapsed_time);
    let current_max_speed = ENEMY_MAX_SPEED * speed_multiplier;
    let current_min_speed = ENEMY_MIN_SPEED * speed_multiplier;

    spawn_timer.timer.tick(time.delta());

    if spawn_timer.timer.just_finished() {
        let window = window_query.single();
        let mut rng = rand::thread_rng();

        // Calculate spawn area within borders
        let spawn_width = window.width() - 2.0 * (BORDER_WIDTH + ENEMY_SPAWN_MARGIN);
        let spawn_height = window.height() - 2.0 * (BORDER_WIDTH + ENEMY_SPAWN_MARGIN);

        let x = rng.gen_range(-spawn_width / 2.0..spawn_width / 2.0);
        let y = rng.gen_range(-spawn_height / 2.0..spawn_height / 2.0);

        let mut rng = rand::thread_rng();
        let hue = rng.gen_range(0.0..360.0);
        let enemy_color = Color::hsl(hue, 0.8, 0.7);

        commands.spawn((
            Enemy {
                health: ENEMY_HEALTH,
                variant: EnemyVariant::Floater,
                color: enemy_color,
            },
            Transform::from_xyz(x, y, 0.0),
            Velocity(Vec2::new(
                rng.gen_range(current_min_speed..current_max_speed),
                rng.gen_range(current_min_speed..current_max_speed),
            )),
            EnemySpeed::default(),
            Growing {
                timer: Timer::from_seconds(ENEMY_GROWTH_TIME, TimerMode::Once),
            },
            GameplayObject,
        ));
    }
}

// Add growth component
#[derive(Component)]
struct Growing {
    timer: Timer,
}

// Update enemy spawn to include growth
fn update_enemy_growth(
    mut commands: Commands,
    mut query: Query<(Entity, &mut Growing)>,
    time: Res<Time>,
) {
    for (entity, mut growing) in &mut query {
        growing.timer.tick(time.delta());
        if growing.timer.finished() {
            commands.entity(entity).remove::<Growing>();
        }
    }
}

// Update enemy drawing to add more visual detail
fn draw_enemies(mut gizmos: Gizmos, query: Query<(&Transform, &Enemy, Option<&Growing>)>) {
    for (transform, enemy, growing) in &query {
        let pos = transform.translation.truncate();
        let health_factor = enemy.health / ENEMY_HEALTH;

        let (scale, alpha) = if let Some(growing) = growing {
            let progress = growing.timer.fraction();
            let scale = ENEMY_MIN_SCALE + (1.0 - ENEMY_MIN_SCALE) * progress;
            (scale, progress)
        } else {
            (1.0, 1.0)
        };

        match enemy.variant {
            EnemyVariant::Floater => {
                let base_color = enemy.color;
                let dark_color: Color = Hsla::from(base_color).with_lightness(0.3).into();

                // Main body - darken with damage
                let body_color: Color = Hsla::from(base_color)
                    .with_lightness(0.7 * health_factor)
                    .into();
                gizmos.circle_2d(pos, ENEMY_RADIUS * scale, body_color.with_alpha(alpha));

                // Inner ring
                gizmos.circle_2d(
                    pos,
                    ENEMY_RADIUS * 0.7 * scale,
                    dark_color.with_alpha(alpha),
                );

                // Core
                gizmos.circle_2d(
                    pos,
                    ENEMY_RADIUS * 0.3 * scale,
                    Color::WHITE.with_alpha(alpha),
                );

                // Spikes
                let spikes = 8;
                for i in 0..spikes {
                    let angle = i as f32 * std::f32::consts::TAU / spikes as f32;
                    let dir = Vec2::new(angle.cos(), angle.sin());
                    let inner = pos + dir * ENEMY_RADIUS * 0.8 * scale;
                    let outer = pos + dir * ENEMY_RADIUS * 1.2 * scale;
                    gizmos.line_2d(inner, outer, dark_color.with_alpha(alpha * 0.8));
                }
            }
            EnemyVariant::Seeker => {
                // Triangular body
                let forward = Vec2::Y * scale;
                let points = [
                    pos + forward * ENEMY_RADIUS * 1.2,
                    pos + forward.rotate(Vec2::from_angle(2.3)) * ENEMY_RADIUS,
                    pos + forward.rotate(Vec2::from_angle(-2.3)) * ENEMY_RADIUS,
                ];

                // Fill
                gizmos.circle_2d(
                    pos,
                    ENEMY_RADIUS * 0.7 * scale,
                    ORANGE.with_alpha(alpha * 0.5),
                );

                // Outline
                for i in 0..points.len() {
                    let start = points[i];
                    let end = points[(i + 1) % points.len()];
                    gizmos.line_2d(start, end, ORANGE.with_alpha(alpha));
                }

                // Core
                gizmos.circle_2d(pos, ENEMY_RADIUS * 0.3 * scale, YELLOW.with_alpha(alpha));
            }
        }
    }
}

// Add function to spawn explosions
fn spawn_explosion(commands: &mut Commands, pos: Vec2, explosion_type: ExplosionType) {
    let (particles, min_speed, max_speed, min_size, max_size, lifetime) = match &explosion_type {
        ExplosionType::Ship => (
            EXPLOSION_PARTICLES,
            EXPLOSION_MIN_SPEED,
            EXPLOSION_MAX_SPEED,
            EXPLOSION_MIN_SIZE,
            EXPLOSION_MAX_SIZE,
            EXPLOSION_LIFETIME,
        ),
        ExplosionType::Enemy { .. } => (
            ENEMY_EXPLOSION_PARTICLES,
            ENEMY_EXPLOSION_MIN_SPEED,
            ENEMY_EXPLOSION_MAX_SPEED,
            ENEMY_EXPLOSION_MIN_SIZE,
            ENEMY_EXPLOSION_MAX_SIZE,
            ENEMY_EXPLOSION_LIFETIME,
        ),
    };

    let mut rng = rand::thread_rng();
    for _ in 0..particles {
        let angle = rng.gen_range(0.0..std::f32::consts::TAU);
        let speed = rng.gen_range(min_speed..max_speed);
        let velocity = Vec2::new(angle.cos(), angle.sin()) * speed;

        commands.spawn((
            ExplosionParticle {
                lifetime: Timer::from_seconds(lifetime, TimerMode::Once),
                velocity,
                size: rng.gen_range(min_size..max_size),
            },
            Transform::from_xyz(pos.x, pos.y, 0.0),
            explosion_type.clone(),
            GameplayObject,
        ));
    }
}

fn check_bubble_enemy_collision(
    mut commands: Commands,
    bubble_query: Query<(Entity, &Transform), With<Bubble>>,
    mut enemy_query: Query<(Entity, &Transform, &mut Enemy, Option<&Growing>)>,
    mut enemy_destroyed: EventWriter<EnemyDestroyed>,
    mut enemy_hit: EventWriter<EnemyHit>,
) {
    let mut destroyed_enemies: Vec<Entity> = Vec::new();
    let mut destroyed_bubbles: Vec<Entity> = Vec::new();

    for (bubble_entity, bubble_transform) in bubble_query.iter() {
        if destroyed_bubbles.contains(&bubble_entity) {
            continue;
        }

        let bubble_pos = bubble_transform.translation.truncate();

        for (enemy_entity, enemy_transform, mut enemy, growing) in enemy_query.iter_mut() {
            // Skip growing enemies
            if growing.is_some() {
                continue;
            }

            if destroyed_enemies.contains(&enemy_entity) {
                continue;
            }

            let enemy_pos = enemy_transform.translation.truncate();

            if bubble_pos.distance(enemy_pos) < ENEMY_RADIUS {
                enemy.health -= BUBBLE_DAMAGE;
                destroyed_bubbles.push(bubble_entity);
                enemy_hit.send(EnemyHit);

                if enemy.health <= 0.0 {
                    destroyed_enemies.push(enemy_entity);
                }
                break; // Bubble can only hit one enemy
            }
        }
    }

    // Spawn explosions for destroyed enemies
    for entity in &destroyed_enemies {
        if let Ok((_, transform, enemy, ..)) = enemy_query.get(*entity) {
            spawn_explosion(
                &mut commands,
                transform.translation.truncate(),
                ExplosionType::Enemy {
                    color: enemy.color.into(),
                },
            );
        }
    }

    // Send event for each destroyed enemy
    for _ in &destroyed_enemies {
        enemy_destroyed.send(EnemyDestroyed);
    }

    // Despawn all at once after collision checks
    for entity in destroyed_bubbles {
        commands.entity(entity).despawn();
    }
    for entity in destroyed_enemies {
        commands.entity(entity).despawn();
    }
}

fn update_bubble_lifetime(
    mut commands: Commands,
    mut bubbles: Query<(Entity, &mut Bubble)>,
    time: Res<Time>,
) {
    for (entity, mut bubble) in &mut bubbles {
        bubble.lifetime.tick(time.delta());
        if bubble.lifetime.finished() {
            commands.entity(entity).despawn();
        }
    }
}

fn handle_ship_border(
    mut ship_query: Query<(&mut Ship, &Transform, &mut Velocity)>,
    window_query: Query<&Window>,
    mut ship_bounced: EventWriter<ShipBounced>,
) {
    if let Ok((mut ship, transform, mut velocity)) = ship_query.get_single_mut() {
        let window = window_query.single();
        let impact_damage = BORDER_DAMAGE; // Fixed damage on impact
        let bounce_force = BORDER_BOUNCE_FORCE;
        let border_width = BORDER_WIDTH;

        let pos = transform.translation;
        let half_width = window.width() / 2.0 - border_width;
        let half_height = window.height() / 2.0 - border_width;

        // Check if ship just entered the border zone
        if pos.x.abs() > half_width || pos.y.abs() > half_height {
            // Only apply damage if ship is moving towards the border
            let to_center = -pos.truncate().normalize();
            if velocity.0.dot(to_center) < 0.0 {
                ship.health -= impact_damage;
                velocity.0 += to_center * bounce_force;
                ship_bounced.send(ShipBounced);
            }
        }
    }
}

fn check_game_over(ship_query: Query<&Ship>, mut next_state: ResMut<NextState<GameState>>) {
    if let Ok(ship) = ship_query.get_single() {
        if ship.health <= 0.0 {
            next_state.set(GameState::Dying);
        }
    }
}

fn spawn_game_over_ui(mut commands: Commands) {
    commands
        .spawn((
            Node {
                width: Val::Percent(100.0),
                height: Val::Percent(100.0),
                align_items: AlignItems::Center,
                justify_content: JustifyContent::Center,
                flex_direction: FlexDirection::Column,
                ..default()
            },
            BackgroundColor(Color::NONE),
            GameOverUI,
        ))
        .with_children(|parent| {
            // Game Over Text
            parent.spawn(Text::new("Game Over"));

            // Replay Button
            parent
                .spawn((
                    Button,
                    Node {
                        width: Val::Px(150.0),
                        height: Val::Px(50.0),
                        margin: UiRect::all(Val::Px(20.0)),
                        justify_content: JustifyContent::Center,
                        align_items: AlignItems::Center,
                        ..default()
                    },
                    BackgroundColor(Color::srgb(0.15, 0.15, 0.15)),
                    ReplayButton,
                ))
                .with_children(|parent| {
                    parent.spawn(Text::new("Replay"));
                });
        });
}

#[derive(Component)]
struct ReplayButton;

fn handle_replay_button(
    mut next_state: ResMut<NextState<GameState>>,
    mut timer: ResMut<StartingTimer>,
    mut interaction_query: Query<&Interaction, (Changed<Interaction>, With<ReplayButton>)>,
) {
    for interaction in &mut interaction_query {
        if *interaction == Interaction::Pressed {
            next_state.set(GameState::Starting);
            timer.reset();
        }
    }
}

// Add cleanup system for game over UI
fn cleanup_game_over_ui(mut commands: Commands, query: Query<Entity, With<GameOverUI>>) {
    for entity in &query {
        commands.entity(entity).despawn_recursive();
    }
}

// Add marker component for gameplay entities
#[derive(Component)]
struct GameplayObject;

// Add cleanup system
fn cleanup_gameplay(mut commands: Commands, query: Query<Entity, With<GameplayObject>>) {
    for entity in &query {
        commands.entity(entity).despawn();
    }
}

fn handle_starting_state(
    mut timer: ResMut<StartingTimer>,
    mut next_state: ResMut<NextState<GameState>>,
    time: Res<Time>,
) {
    timer.tick(time.delta());
    if timer.finished() {
        next_state.set(GameState::Playing);
    }
}

// Add marker component for the get ready text
#[derive(Component)]
struct GetReadyText;

// Add system to spawn the get ready text
fn spawn_get_ready_text(mut commands: Commands) {
    commands
        .spawn((
            Node {
                width: Val::Percent(100.0),
                height: Val::Percent(100.0),
                align_items: AlignItems::Center,
                justify_content: JustifyContent::Center,
                ..default()
            },
            GetReadyText,
        ))
        .with_children(|parent| {
            parent.spawn(Text::new("Get Ready!"));
        });
}

// Add system to cleanup the get ready text
fn cleanup_get_ready_text(mut commands: Commands, query: Query<Entity, With<GetReadyText>>) {
    for entity in &query {
        commands.entity(entity).despawn_recursive();
    }
}

fn handle_ship_enemy_collision(
    mut ship_query: Query<(&mut Ship, &Transform, &mut Velocity)>,
    enemy_query: Query<(&Transform, Option<&Growing>), With<Enemy>>,
    mut ship_bounced: EventWriter<ShipBounced>,
) {
    if let Ok((mut ship, ship_transform, mut ship_vel)) = ship_query.get_single_mut() {
        let ship_pos = ship_transform.translation.truncate();

        for (enemy_transform, growing) in &enemy_query {
            // Skip collision if enemy is still growing
            if growing.is_some() {
                continue;
            }

            let enemy_pos = enemy_transform.translation.truncate();
            let collision_radius = SHIP_RADIUS + ENEMY_RADIUS;
            let impact_damage = ENEMY_COLLISION_DAMAGE;
            let bounce_force = ENEMY_COLLISION_FORCE;

            if ship_pos.distance(enemy_pos) < collision_radius {
                // Calculate bounce direction
                let bounce_dir = (ship_pos - enemy_pos).normalize();
                ship_vel.0 += bounce_dir * bounce_force;
                ship.health -= impact_damage;
                ship_bounced.send(ShipBounced);
                break; // Only handle one collision per frame
            }
        }
    }
}

// Add explosion particle component
#[derive(Component)]
struct ExplosionParticle {
    lifetime: Timer,
    velocity: Vec2,
    size: f32,
}

// Update ship explosion spawn to use the new system
fn spawn_ship_explosion(ship_query: Query<&Transform, With<Ship>>, mut commands: Commands) {
    if let Ok(transform) = ship_query.get_single() {
        spawn_explosion(
            &mut commands,
            transform.translation.truncate(),
            ExplosionType::Ship,
        );
    }
}

// Update system to update explosion particles
fn update_explosion(
    mut commands: Commands,
    mut particles: Query<(Entity, &mut Transform, &mut ExplosionParticle)>,
    time: Res<Time>,
) {
    for (entity, mut transform, mut particle) in &mut particles {
        particle.lifetime.tick(time.delta());

        if particle.lifetime.finished() {
            commands.entity(entity).despawn();
        } else {
            transform.translation += particle.velocity.extend(0.0) * time.delta_secs();
            particle.velocity *= EXPLOSION_DRAG;
        }
    }
}

// Update draw_explosion to handle different explosion types
fn draw_explosion(
    mut gizmos: Gizmos,
    query: Query<(&Transform, &ExplosionParticle, &ExplosionType)>,
) {
    for (transform, particle, explosion_type) in &query {
        let pos = transform.translation.truncate();
        let alpha = particle.lifetime.fraction_remaining();
        let color = match explosion_type {
            ExplosionType::Ship => Color::srgba(1.0, 0.5, 0.0, alpha),
            ExplosionType::Enemy { color } => Color::from(*color).with_alpha(alpha),
        };
        gizmos.circle_2d(pos, particle.size, color);
    }
}

// Add system to regenerate bubble supply
fn regenerate_bubble_supply(mut query: Query<&mut Ship>, time: Res<Time>) {
    if let Ok(mut ship) = query.get_single_mut() {
        ship.bubble_supply =
            (ship.bubble_supply + BUBBLE_REGEN_RATE * time.delta_secs()).min(MAX_BUBBLE_SUPPLY);
    }
}

// Add new system for enemy border bouncing
fn handle_enemy_border(
    mut enemy_query: Query<(&Transform, &mut Velocity, &mut EnemySpeed), With<Enemy>>,
    window_query: Query<&Window>,
    spawn_timer: Res<EnemySpawnTimer>,
) {
    let window = window_query.single();
    let border_width = BORDER_WIDTH;
    let half_width = window.width() / 2.0 - border_width;
    let half_height = window.height() / 2.0 - border_width;

    let speed_multiplier = get_enemy_speed_multiplier(spawn_timer.elapsed_time);

    for (transform, mut velocity, mut speed) in enemy_query.iter_mut() {
        let pos = transform.translation;

        if pos.x.abs() > half_width || pos.y.abs() > half_height {
            let to_center = -pos.truncate().normalize();

            // Increase speed gradually
            speed.current_speed =
                (speed.current_speed + speed.acceleration).min(speed.max_speed * speed_multiplier);

            velocity.0 = to_center * speed.current_speed;
        }
    }
}

// Update sound system to use timer
fn handle_bubble_sound(
    mut commands: Commands,
    mut bubble_shot: EventReader<BubbleShot>,
    mut query: Query<&mut ShootingState>,
    audio: Res<GameAudio>,
    time: Res<Time>,
) {
    if let Ok(mut shooting) = query.get_single_mut() {
        shooting.sound_timer.tick(time.delta());

        if shooting.sound_timer.finished() {
            for _ in bubble_shot.read() {
                commands.spawn((
                    AudioPlayer::new(audio.bubble_shoot.clone()),
                    PlaybackSettings::DESPAWN,
                ));
                shooting.sound_timer.reset();
                break; // Only play one sound per timer tick
            }
        }
    }
}

// Add system to handle enemy death sound
fn handle_enemy_death_sound(
    mut commands: Commands,
    mut enemy_destroyed: EventReader<EnemyDestroyed>,
    audio: Res<GameAudio>,
) {
    for _ in enemy_destroyed.read() {
        commands.spawn((
            AudioPlayer::new(audio.pop.clone()),
            PlaybackSettings::DESPAWN,
        ));
    }
}

fn handle_ship_bounce_sound(
    mut commands: Commands,
    mut ship_bounced: EventReader<ShipBounced>,
    audio: Res<GameAudio>,
) {
    for _ in ship_bounced.read() {
        commands.spawn((
            AudioPlayer::new(audio.bounce.clone()),
            PlaybackSettings::DESPAWN,
        ));
    }
}

fn setup_game_round(
    mut commands: Commands,
    mut spawn_timer: ResMut<EnemySpawnTimer>,
    mut death_timer: ResMut<DeathTimer>,
    mut score: ResMut<Score>,
) {
    commands.spawn((
        Ship {
            health: SHIP_HEALTH,
            bubble_supply: BUBBLE_MAX_SUPPLY,
        },
        Transform::from_xyz(0.0, 0.0, 0.0),
        Velocity::default(),
        AimControl::default(),
        ShootingState::default(),
        GameplayObject,
    ));

    // Reset enemy spawn timer
    spawn_timer.elapsed_time = 0.0;
    spawn_timer.timer.set_duration(Duration::from_secs_f32(3.0));

    death_timer.reset();
    *score = Score::default();
}

fn handle_death_timer(
    mut timer: ResMut<DeathTimer>,
    time: Res<Time>,
    mut next_state: ResMut<NextState<GameState>>,
) {
    timer.tick(time.delta());
    if timer.finished() {
        next_state.set(GameState::GameOver);
    }
}

// Update enemy hit sound handler to use timer
fn handle_enemy_hit_sound(
    mut commands: Commands,
    mut enemy_hit: EventReader<EnemyHit>,
    mut drip_timer: ResMut<DripTimer>,
    audio: Res<GameAudio>,
    time: Res<Time>,
) {
    drip_timer.tick(time.delta());

    if drip_timer.finished() {
        for _ in enemy_hit.read() {
            commands.spawn((
                AudioPlayer::new(audio.drip.clone()),
                PlaybackSettings::DESPAWN,
            ));
            drip_timer.reset();
            break; // Only play one sound per timer tick
        }
    }
}
