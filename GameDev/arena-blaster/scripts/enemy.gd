extends CharacterBody2D
class_name Enemy

signal died(enemy_type: StringName, score_value: int, death_position: Vector2)
signal projectile_requested(origin: Vector2, direction: Vector2, damage: float, speed: float, from_enemy: bool)

const CONTACT_COOLDOWN := 0.8

var enemy_type: StringName = &"melee"

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

var contact_cooldown_left := 0.0
var ranged_cooldown_left := 0.0
var flash_left := 0.0

var sprite: Sprite2D


func _ready() -> void:
	_build_visuals()
	_apply_texture()


func _process(delta: float) -> void:
	if flash_left > 0.0:
		flash_left -= delta
		if flash_left <= 0.0 and sprite:
			sprite.modulate = Color.WHITE


func _physics_process(delta: float) -> void:
	contact_cooldown_left = maxf(contact_cooldown_left - delta, 0.0)
	ranged_cooldown_left = maxf(ranged_cooldown_left - delta, 0.0)

	if not is_instance_valid(target):
		velocity = Vector2.ZERO
		return

	var to_target := target.global_position - global_position
	var direction := to_target.normalized()

	if enemy_type == &"ranged":
		if to_target.length() < 230.0:
			direction = -direction
		elif to_target.length() < 340.0:
			direction = Vector2.ZERO

	velocity = direction * move_speed
	move_and_slide()

	global_position.x = clampf(global_position.x, arena_rect.position.x + radius, arena_rect.end.x - radius)
	global_position.y = clampf(global_position.y, arena_rect.position.y + radius, arena_rect.end.y - radius)

	if enemy_type == &"ranged" or enemy_type == &"boss":
		if ranged_cooldown_left <= 0.0 and to_target.length() > 8.0:
			projectile_requested.emit(global_position, to_target.normalized(), ranged_damage, projectile_speed, true)
			ranged_cooldown_left = ranged_interval


func configure(type_name: StringName) -> void:
	enemy_type = type_name

	match enemy_type:
		&"melee":
			max_hp = 20.0
			hp = 20.0
			move_speed = 400.0
			contact_damage = 15.0
			ranged_damage = 0.0
			ranged_interval = 2.0
			score_value = 10
			radius = 16.0
		&"ranged":
			max_hp = 40.0
			hp = 40.0
			move_speed = 200.0
			contact_damage = 5.0
			ranged_damage = 8.0
			ranged_interval = 2.0
			score_value = 20
			radius = 18.0
		&"boss":
			max_hp = 220.0
			hp = 220.0
			move_speed = 250.0
			contact_damage = 20.0
			ranged_damage = 12.0
			ranged_interval = 1.2
			score_value = 120
			radius = 28.0

	if sprite:
		_apply_texture()


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
