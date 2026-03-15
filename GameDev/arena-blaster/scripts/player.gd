extends CharacterBody2D
class_name Player

signal shoot_requested(origin: Vector2, direction: Vector2, damage: float)
signal health_changed(current_hp: float, max_hp: float, shield_hp: float)
signal died

const BASE_HP := 100.0
const BASE_SPEED := 300.0
const BASE_DAMAGE := 10.0
const BASE_FIRE_INTERVAL := 0.5
const RADIUS := 18.0
const BUFF_DURATION := 5.0
const MANUAL_AIM_MIN_DISTANCE_SQ := 64.0
const DASH_SPEED := 980.0
const DASH_DURATION := 0.15
const DASH_COOLDOWN := 1.2

var max_hp := BASE_HP
var hp := BASE_HP
var shield_hp := 0.0
var temporary_shield_hp := 0.0

var speed_multiplier := 1.0
var fire_rate_multiplier := 1.0

var speed_buff_left := 0.0
var fire_rate_buff_left := 0.0
var shield_buff_left := 0.0

var shoot_cooldown := 0.0
var arena_rect := Rect2(Vector2.ZERO, Vector2(1920.0, 1080.0))
var target_provider: Callable
var last_move_direction := Vector2.RIGHT
var dash_direction := Vector2.RIGHT
var dash_time_left := 0.0
var dash_cooldown_left := 0.0

var sprite: Sprite2D


func _ready() -> void:
	_build_visuals()
	health_changed.emit(hp, max_hp, shield_hp)


func _physics_process(delta: float) -> void:
	_update_buffs(delta)
	_update_dash_state(delta)
	_process_movement()
	_process_auto_fire(delta)


func _update_dash_state(delta: float) -> void:
	dash_time_left = maxf(0.0, dash_time_left - delta)
	dash_cooldown_left = maxf(0.0, dash_cooldown_left - delta)


func _build_visuals() -> void:
	sprite = Sprite2D.new()
	sprite.texture = preload("res://assets/kenney_topdown_shooter/PNG/Man Blue/manBlue_gun.png")
	add_child(sprite)

	var hitbox := CollisionShape2D.new()
	var shape := CircleShape2D.new()
	shape.radius = RADIUS
	hitbox.shape = shape
	add_child(hitbox)


func _process_movement() -> void:
	var input_vector := Vector2(
		Input.get_action_strength("move_right") - Input.get_action_strength("move_left"),
		Input.get_action_strength("move_down") - Input.get_action_strength("move_up")
	)

	if input_vector.length() > 1.0:
		input_vector = input_vector.normalized()

	if input_vector.length_squared() > 0.0001:
		last_move_direction = input_vector.normalized()

	if dash_time_left <= 0.0 and dash_cooldown_left <= 0.0 and Input.is_action_just_pressed("dash"):
		dash_direction = last_move_direction
		dash_time_left = DASH_DURATION
		dash_cooldown_left = DASH_COOLDOWN

	if dash_time_left > 0.0:
		velocity = dash_direction * DASH_SPEED
	else:
		velocity = input_vector * BASE_SPEED * speed_multiplier

	move_and_slide()

	global_position.x = clampf(global_position.x, arena_rect.position.x + RADIUS, arena_rect.end.x - RADIUS)
	global_position.y = clampf(global_position.y, arena_rect.position.y + RADIUS, arena_rect.end.y - RADIUS)


func _process_auto_fire(delta: float) -> void:
	shoot_cooldown = maxf(shoot_cooldown - delta, 0.0)
	if shoot_cooldown > 0.0:
		return

	var manual_direction := _get_manual_aim_direction()
	if manual_direction != Vector2.ZERO:
		shoot_requested.emit(global_position, manual_direction, BASE_DAMAGE)
		shoot_cooldown = BASE_FIRE_INTERVAL / fire_rate_multiplier
		return

	if not target_provider.is_valid():
		return

	var target_point: Variant = target_provider.call()
	if not (target_point is Vector2):
		return

	var direction := (target_point as Vector2) - global_position
	if direction.length_squared() <= 0.0001:
		return

	shoot_requested.emit(global_position, direction.normalized(), BASE_DAMAGE)
	shoot_cooldown = BASE_FIRE_INTERVAL / fire_rate_multiplier


func _get_manual_aim_direction() -> Vector2:
	if not Input.is_mouse_button_pressed(MOUSE_BUTTON_LEFT):
		return Vector2.ZERO

	var direction := get_global_mouse_position() - global_position
	if direction.length_squared() <= MANUAL_AIM_MIN_DISTANCE_SQ:
		return Vector2.ZERO

	return direction.normalized()


func _update_buffs(delta: float) -> void:
	if speed_buff_left > 0.0:
		speed_buff_left -= delta
		if speed_buff_left <= 0.0:
			speed_multiplier = 1.0

	if fire_rate_buff_left > 0.0:
		fire_rate_buff_left -= delta
		if fire_rate_buff_left <= 0.0:
			fire_rate_multiplier = 1.0

	if shield_buff_left > 0.0:
		shield_buff_left -= delta
		if shield_buff_left <= 0.0 and temporary_shield_hp > 0.0:
			shield_hp = maxf(0.0, shield_hp - temporary_shield_hp)
			temporary_shield_hp = 0.0
			health_changed.emit(hp, max_hp, shield_hp)


func set_arena_size(size: Vector2) -> void:
	arena_rect = Rect2(Vector2.ZERO, size)


func apply_damage(amount: float) -> void:
	var incoming := amount

	if shield_hp > 0.0:
		var absorbed := minf(shield_hp, incoming)
		shield_hp -= absorbed
		incoming -= absorbed

		if temporary_shield_hp > 0.0:
			var absorbed_temp := minf(temporary_shield_hp, absorbed)
			temporary_shield_hp -= absorbed_temp

	if incoming > 0.0:
		hp = maxf(0.0, hp - incoming)

	health_changed.emit(hp, max_hp, shield_hp)
	if hp <= 0.0:
		died.emit()


func apply_buff(buff_type: StringName) -> void:
	match buff_type:
		&"speed":
			speed_multiplier = 1.5
			speed_buff_left = BUFF_DURATION
		&"fire_rate":
			fire_rate_multiplier = 2.0
			fire_rate_buff_left = BUFF_DURATION
		&"shield":
			shield_hp += 50.0
			temporary_shield_hp += 50.0
			shield_buff_left = BUFF_DURATION

	health_changed.emit(hp, max_hp, shield_hp)


func get_buff_status_text() -> String:
	var statuses: Array[String] = []
	if speed_buff_left > 0.0:
		statuses.append("Скорость x1.5: %.1fs" % speed_buff_left)
	if fire_rate_buff_left > 0.0:
		statuses.append("Огонь x2: %.1fs" % fire_rate_buff_left)
	if shield_buff_left > 0.0:
		statuses.append("Щит +50: %.1fs" % shield_buff_left)

	if statuses.is_empty():
		return "Баффы: нет"
	return "Баффы: " + ", ".join(statuses)


func get_collision_radius() -> float:
	return RADIUS


func is_dash_invulnerable() -> bool:
	return dash_time_left > 0.0


func get_dash_status_text() -> String:
	if dash_time_left > 0.0:
		return "Рывок: АКТИВЕН"
	if dash_cooldown_left > 0.0:
		return "Рывок: %.1fs" % dash_cooldown_left
	return "Рывок: READY"
