<?php
// Database credentials from environment (docker-compose)
$host     = getenv('MYSQL_HOST')     ?: 'mysql';
$dbname   = getenv('MYSQL_DATABASE') ?: 'testdb';
$user     = getenv('MYSQL_USER')     ?: 'user';
$password = getenv('MYSQL_PASSWORD') ?: 'password';

$connection_success = false;
$tables = [];
$error = '';

try {
    $pdo = new PDO(
        "mysql:host=$host;dbname=$dbname;charset=utf8mb4",
        $user,
        $password,
        [
            PDO::ATTR_ERRMODE            => PDO::ERRMODE_EXCEPTION,
            PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
        ]
    );

    $connection_success = true;
    $stmt = $pdo->query("SHOW TABLES");
    $tables = $stmt->fetchAll(PDO::FETCH_COLUMN);

} catch (PDOException $e) {
    $error = $e->getMessage();
}

// Some fun random quotes
$quotes = [
    "Code is like humor. When you have to explain it, it’s bad. — Cory House",
    "First, solve the problem. Then, write the code. — John Johnson",
    "Any fool can write code that a computer can understand. Good programmers write code that humans can understand. — Martin Fowler",
    "The best error message is the one that never shows up. — Thomas Fuchs",
    "Talk is cheap. Show me the code. — Linus Torvalds",
];

$random_quote = $quotes[array_rand($quotes)];

// Server info
$php_version = phpversion();
$server_software = $_SERVER['SERVER_SOFTWARE'] ?? 'Nginx + PHP-FPM';
$request_time = date('Y-m-d H:i:s');
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>LNMP Lab - Docker Compose Demo</title>
    <style>
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: linear-gradient(135deg, #667eea, #764ba2);
            color: #fff;
            margin: 0;
            padding: 0;
            min-height: 100vh;
            display: flex;
            justify-content: center;
            align-items: center;
        }
        .container {
            background: rgba(0, 0, 0, 0.65);
            backdrop-filter: blur(10px);
            border-radius: 16px;
            padding: 2.5rem;
            max-width: 700px;
            box-shadow: 0 15px 35px rgba(0,0,0,0.4);
            text-align: center;
        }
        h1 { margin: 0 0 1.5rem; font-size: 2.8rem; }
        .status { font-size: 1.4rem; margin: 1rem 0; }
        .success { color: #00ff9d; }
        .error   { color: #ff6b6b; }
        .quote {
            font-style: italic;
            margin: 2rem 0;
            padding: 1rem;
            border-left: 5px solid #00ff9d;
            background: rgba(255,255,255,0.08);
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 1.5rem 0;
        }
        th, td {
            padding: 0.8rem;
            border: 1px solid rgba(255,255,255,0.2);
        }
        th { background: rgba(255,255,255,0.1); }
        .info { font-size: 0.95rem; opacity: 0.9; margin-top: 2rem; }
        footer { margin-top: 2rem; font-size: 0.9rem; opacity: 0.7; }
    </style>
</head>
<body>

<div class="container">
    <h1>LNMP Stack – Docker Compose Lab</h1>

    <div class="status">
        <?php if ($connection_success): ?>
            <span class="success">? MySQL connection successful!</span>
        <?php else: ?>
            <span class="error">? MySQL connection failed</span>
        <?php endif; ?>
    </div>

    <?php if ($error): ?>
        <p style="color:#ff6b6b; background:rgba(0,0,0,0.3); padding:1rem; border-radius:8px;">
            <?= htmlspecialchars($error) ?>
        </p>
    <?php endif; ?>

    <?php if ($connection_success): ?>
        <div class="quote">
            “<?= htmlspecialchars($random_quote) ?>”
        </div>

        <h3>Database: <code><?= htmlspecialchars($dbname) ?></code></h3>

        <?php if (count($tables) > 0): ?>
            <h3>Tables found (<?= count($tables) ?>):</h3>
            <table>
                <tr><th>Table Name</th></tr>
                <?php foreach ($tables as $table): ?>
                    <tr><td><?= htmlspecialchars($table) ?></td></tr>
                <?php endforeach; ?>
            </table>
        <?php else: ?>
            <p>No tables yet in the database.<br>
               <small>(You can create one via phpMyAdmin or CLI if you want)</small></p>
        <?php endif; ?>
    <?php endif; ?>

    <div class="info">
        PHP: <?= $php_version ?> | Server: <?= htmlspecialchars($server_software) ?><br>
        Request time: <?= $request_time ?>
    </div>

    <footer>
        LNMP demo • Nginx + PHP-FPM + MySQL • Running in Docker
    </footer>
</div>

</body>
</html>