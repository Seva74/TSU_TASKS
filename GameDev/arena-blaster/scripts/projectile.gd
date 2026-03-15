extends Node2D
class_name Projectile

var velocity := Vector2.ZERO
var damage := 0.0
var from_enemy := false
var radius := 5.0
var lifetime := 2.0


func setup(origin: Vector2, direction: Vector2, speed: float, base_damage: float, enemy_projectile: bool) -> void:
	global_position = origin
	velocity = direction.normalized() * speed
	damage = base_damage
	from_enemy = enemy_projectile
	radius = 5.0 if from_enemy else 4.0
	lifetime = 2.8 if from_enemy else 1.7
	queue_redraw()


func _physics_process(delta: float) -> void:
	global_position += velocity * delta
	lifetime -= delta
	if lifetime <= 0.0:
		queue_free()


func is_outside(arena_rect: Rect2, margin: float = 40.0) -> bool:
	return not arena_rect.grow(margin).has_point(global_position)


func _draw() -> void:
	var color_main := Color(1.0, 0.34, 0.34, 0.95) if from_enemy else Color(0.3, 0.95, 1.0, 0.95)
	draw_circle(Vector2.ZERO, radius, color_main)
	draw_circle(Vector2.ZERO, radius * 0.45, Color(1.0, 1.0, 1.0, 0.9))
