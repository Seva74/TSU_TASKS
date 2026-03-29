extends CharacterBody2D
class_name Enemy

signal died(enemy_type: StringName, score_value: int, death_position: Vector2)
signal projectile_requested(origin: Vector2, direction: Vector2, damage: float, speed: float, from_enemy: bool)

const CONTACT_COOLDOWN := 0.8
const DETECTION_RANGE := 560.0
const LOST_SIGHT_RANGE := 720.0
const RANGED_KEEP_DISTANCE := 240.0
const RANGED_TOO_CLOSE_DISTANCE := 160.0
const WANDER_SPEED_SCALE := 0.45

var enemy_type: StringName = &"melee"
var elite := false

var max_hp := 20.0
var hp := 20.0
var move_speed := 400.0
var contact_damage := 15.0
var ranged_damage := 0.0
var ranged_interval := 2.0
var projectile_speed := 650.0
var score_value := 10
var radius := 16.0

var arena_rect := Rect2(Vector2.ZERO, Vector2(1920.0, 1080.0))
var target: Node2D
var visibility_provider: Callable

var contact_cooldown_left := 0.0
var ranged_cooldown_left := 0.0
var flash_left := 0.0
var last_known_target_position := Vector2.ZERO
var has_last_known_target := false
var wander_direction := Vector2.RIGHT
var wander_time_left := 0.0
var default_modulate: Color = Color.WHITE

var sprite: Sprite2D


func _ready() -> void:
	_build_visuals()
	_apply_texture()
	default_modulate = sprite.modulate if sprite else Color.WHITE
	wander_direction = Vector2.RIGHT.rotated(randf_range(0.0, TAU))
	wander_time_left = randf_range(0.4, 1.4)


func _process(delta: float) -> void:
	if flash_left > 0.0:
		flash_left -= delta
		if flash_left <= 0.0 and sprite:
			sprite.modulate = default_modulate


func _physics_process(delta: float) -> void:
	contact_cooldown_left = maxf(contact_cooldown_left - delta, 0.0)
	ranged_cooldown_left = maxf(ranged_cooldown_left - delta, 0.0)

	if not is_instance_valid(target):
		velocity = Vector2.ZERO
		return

	var to_target := target.global_position - global_position
	var direction := to_target.normalized()
	var distance := to_target.length()
	var can_see_target := _can_see_target()

	if can_see_target:
		last_known_target_position = target.global_position
		has_last_known_target = true
		wander_time_left = 0.0
	elif has_last_known_target and global_position.distance_to(last_known_target_position) > LOST_SIGHT_RANGE:
		has_last_known_target = false

	var move_direction := Vector2.ZERO

	if enemy_type == &"ranged":
		if can_see_target:
			if distance < RANGED_TOO_CLOSE_DISTANCE:
				move_direction = -direction
			elif distance > RANGED_KEEP_DISTANCE:
				move_direction = direction
			else:
				move_direction = direction.orthogonal() * signf(sin(global_position.x * 0.01 + global_position.y * 0.02))
		elif has_last_known_target and global_position.distance_to(last_known_target_position) > 24.0:
			move_direction = (last_known_target_position - global_position).normalized()
		else:
			wander_time_left -= delta
			if wander_time_left <= 0.0:
				wander_direction = Vector2.RIGHT.rotated(randf_range(0.0, TAU))
				wander_time_left = randf_range(0.5, 1.5)
			move_direction = wander_direction
	else:
		if can_see_target:
			move_direction = direction
		elif has_last_known_target and global_position.distance_to(last_known_target_position) > 24.0:
			move_direction = (last_known_target_position - global_position).normalized()
		else:
			wander_time_left -= delta
			if wander_time_left <= 0.0:
				wander_direction = Vector2.RIGHT.rotated(randf_range(0.0, TAU))
				wander_time_left = randf_range(0.7, 1.8)
			move_direction = wander_direction

	var speed_scale := 1.0 if can_see_target else WANDER_SPEED_SCALE
	velocity = move_direction.normalized() * move_speed * speed_scale if move_direction.length_squared() > 0.0001 else Vector2.ZERO
	move_and_slide()

	global_position.x = clampf(global_position.x, arena_rect.position.x + radius, arena_rect.end.x - radius)
	global_position.y = clampf(global_position.y, arena_rect.position.y + radius, arena_rect.end.y - radius)

	if (enemy_type == &"ranged" or enemy_type == &"boss") and can_see_target:
		if ranged_cooldown_left <= 0.0 and distance > 8.0:
			projectile_requested.emit(global_position, to_target.normalized(), ranged_damage, projectile_speed, true)
			ranged_cooldown_left = ranged_interval


func configure(type_name: StringName) -> void:
	enemy_type = type_name
	elite = false

	match enemy_type:
		&"melee":
			max_hp = 16.0
			hp = 16.0
			move_speed = 360.0
			contact_damage = 11.0
			ranged_damage = 0.0
			ranged_interval = 2.0
			score_value = 10
			radius = 16.0
		&"ranged":
			max_hp = 28.0
			hp = 28.0
			move_speed = 220.0
			contact_damage = 4.0
			ranged_damage = 6.0
			ranged_interval = 2.4
			score_value = 20
			radius = 18.0
		&"boss":
			max_hp = 180.0
			hp = 180.0
			move_speed = 235.0
			contact_damage = 15.0
			ranged_damage = 10.0
			ranged_interval = 1.5
			score_value = 120
			radius = 28.0

	default_modulate = Color.WHITE
	if sprite:
		_apply_texture()


func make_elite() -> void:
	if elite:
		return

	elite = true
	max_hp *= 1.45
	hp = max_hp
	move_speed *= 1.12
	contact_damage *= 1.2
	ranged_damage *= 1.2
	score_value = int(round(float(score_value) * 1.8))
	radius += 2.0
	default_modulate = Color(1.0, 0.9, 0.45)
	if sprite:
		sprite.modulate = default_modulate
		if enemy_type == &"boss":
			sprite.scale = Vector2(1.65, 1.65)
		else:
			sprite.scale = Vector2.ONE * 1.08


func set_arena_size(size: Vector2) -> void:
	arena_rect = Rect2(Vector2.ZERO, size)


func take_damage(amount: float) -> void:
	hp -= amount
	flash_left = 0.08
	if sprite:
		sprite.modulate = Color(1.6, 0.5, 0.5)

	if hp <= 0.0:
		died.emit(enemy_type, score_value, global_position)
		queue_free()


func can_deal_contact_damage() -> bool:
	return contact_damage > 0.0 and contact_cooldown_left <= 0.0


func register_contact_damage() -> void:
	contact_cooldown_left = CONTACT_COOLDOWN


func _can_see_target() -> bool:
	if not is_instance_valid(target):
		return false

	if visibility_provider.is_valid():
		return bool(visibility_provider.call())

	return global_position.distance_to(target.global_position) <= DETECTION_RANGE


func get_collision_radius() -> float:
	return radius


func _build_visuals() -> void:
	sprite = Sprite2D.new()
	add_child(sprite)

	var hitbox := CollisionShape2D.new()
	var shape := CircleShape2D.new()
	shape.radius = radius
	hitbox.shape = shape
	add_child(hitbox)


func _apply_texture() -> void:
	match enemy_type:
		&"melee":
			sprite.texture = preload("res://assets/kenney_topdown_shooter/PNG/Zombie 1/zoimbie1_stand.png")
		&"ranged":
			sprite.texture = preload("res://assets/kenney_topdown_shooter/PNG/Robot 1/robot1_gun.png")
		&"boss":
			sprite.texture = preload("res://assets/kenney_topdown_shooter/PNG/Soldier 1/soldier1_machine.png")

	if enemy_type == &"boss":
		sprite.scale = Vector2(1.6, 1.6)
	else:
		sprite.scale = Vector2.ONE
