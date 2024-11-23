#define _POSIX_C_SOURCE 200112L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <sys/select.h>
#include <errno.h>

#define PORT 12345
#define BACKLOG 5
#define BUFFER_SIZE 1024

volatile sig_atomic_t got_sighup = 0;

void handle_signal(int sig) {
    if (sig == SIGHUP) {
        got_sighup = 1;
    }
}

int main() {
    int server_fd, client_fd = -1;
    struct sockaddr_in server_addr;
    fd_set read_fds;
    struct sigaction sa;
    sigset_t sigmask, origmask;
    char buffer[BUFFER_SIZE];

    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        perror("socket");
        exit(EXIT_FAILURE);
    }

    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(PORT);

    if (bind(server_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) == -1) {
        perror("bind");
        close(server_fd);
        exit(EXIT_FAILURE);
    }

    if (listen(server_fd, BACKLOG) == -1) {
        perror("listen");
        close(server_fd);
        exit(EXIT_FAILURE);
    }

    printf("Server is listening on port %d\n", PORT);

    memset(&sa, 0, sizeof(sa));  
    sa.sa_handler = handle_signal; 
    sa.sa_flags = 0; 
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGHUP, &sa, NULL) == -1) {
        perror("sigaction");
        close(server_fd);
        exit(EXIT_FAILURE);
    }

    sigemptyset(&sigmask);
    sigaddset(&sigmask, SIGHUP);
    if (sigprocmask(SIG_BLOCK, &sigmask, &origmask) == -1) {
        perror("sigprocmask");
        close(server_fd);
        exit(EXIT_FAILURE);
    }

    while (1) {
        FD_ZERO(&read_fds);
        FD_SET(server_fd, &read_fds);
        if (client_fd != -1) {
            FD_SET(client_fd, &read_fds);
        }

        int nfds = (client_fd > server_fd ? client_fd : server_fd) + 1;

        int ready = pselect(nfds, &read_fds, NULL, NULL, NULL, &origmask);
        if (ready == -1) {
            if (errno == EINTR) continue;
            perror("pselect");
            break;
        }

        if (got_sighup) {
            printf("Received SIGHUP\n");
            got_sighup = 0;
        }

        if (FD_ISSET(server_fd, &read_fds)) {
            int new_fd = accept(server_fd, NULL, NULL);
            if (new_fd == -1) {
                perror("accept");
                continue;
            }
            printf("New connection received\n");

            if (client_fd == -1) {
                client_fd = new_fd; 
            } else {
                printf("Connection closed (only one connection allowed)\n");
                close(new_fd);
            }
        }

        if (client_fd != -1 && FD_ISSET(client_fd, &read_fds)) {
            ssize_t bytes_read = recv(client_fd, buffer, sizeof(buffer), 0);
            if (bytes_read > 0) {
                printf("Received %zd bytes of data\n", bytes_read);
            } else if (bytes_read == 0) {
                printf("Client disconnected\n");
                close(client_fd);
                client_fd = -1;
            } else {
                perror("recv");
            }
        }
    }

    close(server_fd);
    if (client_fd != -1) close(client_fd);
    return 0;
}
