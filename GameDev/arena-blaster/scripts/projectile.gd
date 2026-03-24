extends Node2D
class_name Projectile

var velocity := Vector2.ZERO
var damage := 0.0
var from_enemy := false
var weapon_type: StringName = &"pistol"
var radius := 5.0
var lifetime := 2.0
var previous_position := Vector2.ZERO
var pierce_remaining := 0
var hit_enemy_ids: Array[int] = []


func setup(origin: Vector2, direction: Vector2, speed: float, base_damage: float, enemy_projectile: bool, weapon_name: StringName = &"pistol", lifetime_override: float = -1.0) -> void:
	global_position = origin
	previous_position = origin
	velocity = direction.normalized() * speed
	damage = base_damage
	from_enemy = enemy_projectile
	weapon_type = weapon_name
	hit_enemy_ids.clear()
	pierce_remaining = 0
	if weapon_type == &"sniper" and not from_enemy:
		pierce_remaining = 4
	radius = 5.0 if from_enemy else 4.0
	if lifetime_override > 0.0:
		lifetime = lifetime_override
	else:
		lifetime = 2.8 if from_enemy else 1.7
	queue_redraw()


func _physics_process(delta: float) -> void:
	previous_position = global_position
	global_position += velocity * delta
	lifetime -= delta
	if lifetime <= 0.0:
		queue_free()


func is_outside(arena_rect: Rect2, margin: float = 40.0) -> bool:
	return not arena_rect.grow(margin).has_point(global_position)


func get_previous_position() -> Vector2:
	return previous_position


func has_hit_enemy(enemy: Node) -> bool:
	return enemy != null and hit_enemy_ids.has(enemy.get_instance_id())


func register_enemy_hit(enemy: Node) -> void:
	if enemy == null:
		return

	var enemy_id := enemy.get_instance_id()
	if not hit_enemy_ids.has(enemy_id):
		hit_enemy_ids.append(enemy_id)


func _draw() -> void:
	var color_main := Color(1.0, 0.34, 0.34, 0.95) if from_enemy else _get_player_projectile_color()
	draw_circle(Vector2.ZERO, radius, color_main)
	draw_circle(Vector2.ZERO, radius * 0.45, Color(1.0, 1.0, 1.0, 0.9))


func _get_player_projectile_color() -> Color:
	match weapon_type:
		&"shotgun":
			return Color(1.0, 0.68, 0.26, 0.96)
		&"flamethrower":
			return Color(1.0, 0.45, 0.18, 0.96)
		_:
			return Color(0.3, 0.95, 1.0, 0.95)
