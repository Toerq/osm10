/**
 * Game of luck: Implementation of the Gamemaster
 *
 * Course: Operating Systems and Multicore Programming - OSM lab
 * assignment 1: game of luck.
 *
 * Author: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 *
 */

#define _XOPEN_SOURCE

#include <stdio.h> /* I/O functions: printf() ... */
#include <stdlib.h> /* rand(), srand() */
#include <unistd.h> /* read(), write() calls */
#include <assert.h> /* assert() */
#include <time.h>   /* time() */
#include <signal.h> /* kill(), raise() and SIG???? */

#include <sys/types.h> /* pid */
#include <sys/wait.h> /* waitpid() */

#include "common.h"

int main(int argc, char *argv[])
{
  int i, seed;

  pid_t pids[NUM_PLAYERS];
  //int results[NUM_PLAYERS];

  /* TODO: Use the following variables in the exec system call. Using the
   * function sprintf and the arg1 variable you can pass the id parameter
   * to the children
   */
  char arg0[] = "./shooter";
  char arg1[10];
  char *args[] = {arg0, arg1, NULL};

  /* TODO: initialize the communication with the players */
  int pfd_parent_to_child[NUM_PLAYERS][2];
  int pfd_child_to_parent[NUM_PLAYERS][2];
  for (i = 0; i < NUM_PLAYERS; i++) {
    if(pipe(pfd_child_to_parent[i]) == -1) { 
      perror("Pipe failed!\n"); 
      exit(EXIT_FAILURE); 
    }
    if(pipe(pfd_parent_to_child[i]) == -1) {
      perror("Pipe failed!\n");
      exit(EXIT_FAILURE);
    }
  }

  for (i = 0; i < NUM_PLAYERS; i++) {
    /* TODO: spawn the processes that simulate the players */
    switch(pids[i] = fork()) {
    case -1:
      perror("PARENT: couldn't create a child process...");
      exit(EXIT_FAILURE);
      break;
    case 0:
      printf("CHILD: My PID is: <%ld>\n", (long) getpid());
      
      close(pfd_parent_to_child[i][1]);
      close(pfd_child_to_parent[i][0]);
      for(int descIndex = 0; descIndex < NUM_PLAYERS; descIndex++) {
	if (descIndex != i) {
	  close(pfd_parent_to_child[descIndex][0]);
	  close(pfd_parent_to_child[descIndex][1]);
	  close(pfd_child_to_parent[descIndex][0]);
	  close(pfd_child_to_parent[descIndex][1]);
	} 
      }

      //shooter(i, pfd_parent_to_child[i][0], pfd_child_to_parent[i][1]);
      dup2(pfd_parent_to_child[i][0], STDIN_FILENO);
      dup2(pfd_child_to_parent[i][1], STDOUT_FILENO);
      sprintf(arg1, "%d", i);
      execv(args[0], args);
      break;
    }
  }

  seed = time(NULL);
  for (i = 0; i < NUM_PLAYERS; i++) {
    seed++;
    /* TODO: send the seed to the players */
    write(pfd_parent_to_child[i][1], &seed, sizeof(int));
  }

  /* TODO: get the dice results from the players, find the winner */
  //int currentLeader = 0;
  int highestScore = 0;
  for (i = 0; i < NUM_PLAYERS; i++) {
    int currentResult = 0;
    read(pfd_child_to_parent[i][0], &currentResult, sizeof(int));
    if(currentResult > highestScore) {
      winner = i;
      highestScore = currentResult;
    }
  }

  printf("master: player %d WINS with a score of: %d\n", winner, highestScore);

  /* TODO: signal the winner */
  kill(pids[winner], SIGUSR1);
  
  /* TODO: signal all players the end of game */
  int pidResult;
  for (i = 0; i < NUM_PLAYERS; i++) {
    kill(pids[i], SIGUSR2);
    //wait(NULL);
    waitpid(pids[i], &pidResult, 0);
    waitstat(pids[i], pidResult);
  }

  printf("master: the game ends\n");

  /* TODO: cleanup resources and exit with success */
  for (i = 0; i < NUM_PLAYERS; i++) {
    close(pfd_parent_to_child[i][0]);
    close(pfd_parent_to_child[i][1]);
    close(pfd_child_to_parent[i][0]);
    close(pfd_child_to_parent[i][1]);
  }

  exit(EXIT_SUCCESS);
  //return 0;
}
