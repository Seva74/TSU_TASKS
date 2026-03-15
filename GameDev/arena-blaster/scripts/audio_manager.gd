extends Node
class_name AudioManager

const MIX_RATE := 22050.0

var rng := RandomNumberGenerator.new()

var music_player: AudioStreamPlayer
var music_playback: AudioStreamGeneratorPlayback
var music_time := 0.0
var lead_phase := 0.0
var bass_phase := 0.0


func _ready() -> void:
	rng.randomize()
	_start_music()


func _process(_delta: float) -> void:
	_fill_music_buffer()


func play_shoot() -> void:
	_play_sfx(0.09, 760.0, -380.0, 0.22, 0.0)


func play_enemy_shoot() -> void:
	_play_sfx(0.12, 430.0, -150.0, 0.2, 0.02)


func play_explosion() -> void:
	_play_sfx(0.24, 180.0, -120.0, 0.35, 0.24)


func play_pickup() -> void:
	_play_sfx(0.16, 500.0, 280.0, 0.24, 0.01)


func play_hurt() -> void:
	_play_sfx(0.14, 220.0, -90.0, 0.2, 0.16)


func play_wave_start() -> void:
	_play_sfx(0.25, 260.0, 210.0, 0.2, 0.0)


func play_win() -> void:
	_play_sfx(0.45, 350.0, 420.0, 0.26, 0.0)


func play_lose() -> void:
	_play_sfx(0.45, 260.0, -180.0, 0.24, 0.08)


func _start_music() -> void:
	music_player = AudioStreamPlayer.new()
	music_player.volume_db = -16.0

	var stream := AudioStreamGenerator.new()
	stream.mix_rate = MIX_RATE
	stream.buffer_length = 0.7
	music_player.stream = stream

	add_child(music_player)
	music_player.play()
	music_playback = music_player.get_stream_playback() as AudioStreamGeneratorPlayback


func _fill_music_buffer() -> void:
	if not is_instance_valid(music_player):
		return

	if music_playback == null:
		music_playback = music_player.get_stream_playback() as AudioStreamGeneratorPlayback
		if music_playback == null:
			return

	var frames := music_playback.get_frames_available()
	for i in frames:
		var pulse := sin(TAU * 0.5 * music_time) * 0.02
		var lead_frequency := 170.0 + 30.0 * sin(TAU * 0.11 * music_time)
		var bass_frequency := 54.0 + 6.0 * sin(TAU * 0.07 * music_time)

		lead_phase += TAU * lead_frequency / MIX_RATE
		bass_phase += TAU * bass_frequency / MIX_RATE

		var lead := sin(lead_phase + 0.35 * sin(TAU * 0.22 * music_time)) * 0.09
		var bass := signf(sin(bass_phase)) * 0.05
		var pad := sin(TAU * 0.03 * music_time) * 0.03

		var sample := clampf(lead + bass + pad + pulse, -0.95, 0.95)
		music_playback.push_frame(Vector2(sample, sample))
		music_time += 1.0 / MIX_RATE


func _play_sfx(duration: float, start_frequency: float, frequency_slide: float, amplitude: float, noise: float) -> void:
	var sfx_player := AudioStreamPlayer.new()
	sfx_player.volume_db = -7.0

	var stream := AudioStreamGenerator.new()
	stream.mix_rate = MIX_RATE
	stream.buffer_length = maxf(0.15, duration + 0.04)
	sfx_player.stream = stream

	add_child(sfx_player)
	sfx_player.play()

	var playback := sfx_player.get_stream_playback() as AudioStreamGeneratorPlayback
	if playback == null:
		sfx_player.queue_free()
		return

	var frame_count := int(MIX_RATE * duration)
	var phase := 0.0

	for i in frame_count:
		var t := float(i) / maxf(float(frame_count - 1), 1.0)
		var frequency := maxf(45.0, start_frequency + frequency_slide * t)
		phase += TAU * frequency / MIX_RATE

		var envelope := pow(1.0 - t, 2.0)
		var sample := sin(phase) * envelope * amplitude
		if noise > 0.0:
			sample += rng.randf_range(-1.0, 1.0) * noise * envelope

		sample = clampf(sample, -1.0, 1.0)
		playback.push_frame(Vector2(sample, sample))

	var cleanup_timer := get_tree().create_timer(duration + 0.2)
	cleanup_timer.timeout.connect(sfx_player.queue_free)
