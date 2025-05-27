import pika
import time
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def connect_to_rabbitmq():
    max_retries = 10
    retry_delay = 5  # seconds
    for attempt in range(max_retries):
        try:
            connection = pika.BlockingConnection(pika.ConnectionParameters('rabbitmq'))
            logger.info("Successfully connected to RabbitMQ")
            return connection
        except pika.exceptions.AMQPConnectionError as e:
            logger.warning(f"Connection attempt {attempt + 1}/{max_retries} failed: {e}")
            if attempt < max_retries - 1:
                time.sleep(retry_delay)
            else:
                logger.error("Failed to connect to RabbitMQ after all retries")
                raise

connection = connect_to_rabbitmq()
channel = connection.channel()

channel.queue_declare(queue='tasks')

def callback(ch, method, properties, body):
    logger.info(f" [x] Received {body}")
    time.sleep(5)    
    logger.info(f" [x] Done {body}")

channel.basic_consume(queue='tasks', on_message_callback=callback, auto_ack=False)

logger.info(' [*] Waiting for messages')
channel.start_consuming()