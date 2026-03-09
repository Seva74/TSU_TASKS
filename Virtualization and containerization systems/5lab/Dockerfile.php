FROM php:8.3-fpm

RUN docker-php-ext-install mysqli pdo pdo_mysql

WORKDIR /var/www/html

RUN chown -R www-data:www-data /var/www/html
