/**
 * Game of luck: Implementation of the Gamemaster
 *
 * Course: Operating Systems and Multicore Programming - OSM lab
 * assignment 1: game of luck.
 *
 * Author: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 *
 */

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
  int child[NUM_PLAYERS];
  int descIndex;
  /* TODO: Use the following variables in the exec system call. Using the
   * function sprintf and the arg1 variable you can pass the id parameter
   * to the children
   */
  
    char arg0[] = "./shooter";
    char arg1[10];
    char *args[] = {arg0, arg1, NULL};
  
  /* TODO: initialize the communication with the players */
  int pfd_parent_to_child[NUM_PLAYERS] [2];
  int pfd_child_to_parent[NUM_PLAYERS] [2];
  for (i = 0; i < NUM_PLAYERS; i++) {
    pipe(pfd_child_to_parent[i]);
    pipe(pfd_parent_to_child[i]);

  }

  for (i = 0; i < NUM_PLAYERS; i++) {
    /* TODO: spawn the processes that simulate the players */
    switch (child[i] = fork()) {
	 
    case -1:
      perror("could not create child process");
      exit(EXIT_FAILURE);
      break;
	 
    case 0:
      // printf("PID of child: <%ld>\n", (long) getpid());
      close(pfd_parent_to_child[i][1]);
      close(pfd_child_to_parent[i][0]);
      for(descIndex = 0; descIndex < NUM_PLAYERS; descIndex++) {
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
      sprintf(arg1,"%d",i);
      execv(args[0], args);
      break;
    }
  }

  close(pfd_parent_to_child[i][0]);
  close(pfd_child_to_parent[i][1]);
  seed = time(NULL);
  for (i = 0; i < NUM_PLAYERS; i++) {
    seed++;
    /* TODO: send the seed to the players */
    write(pfd_parent_to_child[i][1], &seed, sizeof(seed));
  }
  // wait(NULL);
  /* TODO: get the dice results from the players, find the winner */
  int result = 0;
  int highestScore = 0;
  int winner= 0;
  int pidResult;
  for (i = 0; i < NUM_PLAYERS; i++) {
    read(pfd_child_to_parent[i][0], &result, sizeof(result));
    if(result > highestScore) {
      winner = i;
      highestScore = result;
    }

  }
  printf("master: player %d WINS\n", winner);

  /* TODO: signal the winner */
  kill(child[winner],SIGUSR1);
  /* TODO: signal all players the end of game */
  for (i = 0; i < NUM_PLAYERS; i++) {
      kill(child[i], SIGUSR2);
      waitpid(child[i], &pidResult, 0);
      waitstat(child[i], pidResult);

  }

  printf("master: the game ends\n");

  /* TODO: cleanup resources and exit with success */
  for (i = 0; i < NUM_PLAYERS; i++) {

  }
  return 0;
}
