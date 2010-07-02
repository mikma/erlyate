/*
  wrapper.c

  Copyright 2006 Mikael Magnusson

  Erlang port wrapper,
  kill external process if stdin is closed by Erlang
*/

#include<stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>

#if 0
#define DEBUG printf
#else
#define DEBUG(...)
#endif

int loop(int child, int fdin, int fdout)
{
    int maxfd = fdin > fdout ? fdin : fdout;

    while(1) {
	int res;
	fd_set readfds;

	FD_ZERO(&readfds);
	FD_SET(0, &readfds);
	FD_SET(fdin, &readfds);
    
	res = select(maxfd + 1, &readfds, NULL, NULL, NULL);

	if (res < 0) {
	    perror("select");
	    return res;
	}

	if (FD_ISSET(0, &readfds)) {
	    char buf[1024];
	    ssize_t len;

	    len = read(0, buf, sizeof(buf));

	    if (len == 0) {
		DEBUG("stdin closed\n");
		if (kill(child, SIGTERM) < 0) {
		    perror("kill");
		    return 1;
		}
	    } else if (len > 0) {
		res = write(fdout, buf, len);
		if ( res < 0 ) {
		    perror("write");
		    return 1;
		}
	    }
	}

	if (FD_ISSET(fdin, &readfds)) {
	    char buf[1024];
	    ssize_t len;

	    len = read(fdin, buf, sizeof(buf));

	    if (len == 0) {
		pid_t res;
		int status = 0;
		DEBUG("pipe closed\n");
		res = waitpid(child, &status, WNOHANG);

		if (res < 0) {
		    perror("waitpid");
		    return 0;
		} else if (res == 0) {
		    return 0;
		}

		if (WIFEXITED(status)) {
		    return WEXITSTATUS(status);
		} else if (WIFSIGNALED(status)) {
		    DEBUG("child terminated by signal\n");
		    return 1;
		}

		return 0;
	    } else if (len > 0) {
		res = write(1, buf, len);
		if ( res < 0 ) {
		    perror("write");
		    return 1;
		}
	    }
	}
    }
}

int main(int argc, char *argv[])
{
    pid_t pid;
    int pipes[2][2];
    int i;
    char **args;
    const char *directory;

    if (argc < 3) {
	fprintf(stderr, "Usage: %s directory program [args*]\n", argv[0]);
	return 1;
    }

    directory = argv[1];
    if ( chdir(directory) < 0 ) {
	perror("chdir");
	return 1;
    }

    args = (char **)calloc(argc, sizeof(args[0]));
    args[argc - 2] = NULL;
    for (i = 2; i < argc; i++) {
	args[i - 2] = argv[i];
    }

    for (i = 0; i < 2; i++) {
	if (pipe(pipes[i]) < 0) {
	    perror("pipe");
	    return 1;
	}
    }

    pid = fork();
    switch(pid) {
    case 0:
	close(pipes[0][1]);
	close(pipes[1][0]);
	DEBUG("Exec %s\n", argv[1]);
	dup2(pipes[0][0], 0);
	dup2(pipes[1][1], 1);
	dup2(pipes[1][1], 2);
	execvp(args[0], args);
	perror("exec");
	return 1;

    case -1:
	perror("fork");
	return 1;

    default:
	close(pipes[0][0]);
	close(pipes[1][1]);
	DEBUG("Pid %d\n", pid);

	return loop(pid, pipes[1][0], pipes[0][1]);
    }

    return 0;
}
