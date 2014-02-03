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
#include <string.h>

#include <sys/types.h> /* pid */
#include <sys/wait.h> /* waitpid() */

#include "common.h"

int main(int argc, char *argv[])
{
  int i, seed;
  int pids[NUM_PLAYERS];
  int results[NUM_PLAYERS];
  /* TODO: Use the following variables in the exec system call. Using the
   * function sprintf and the arg1 variable you can pass the id parameter
   * to the children
   */
  /*
    char arg0[] = "./shooter";
    char arg1[10];
    char *args[] = {arg0, arg1, NULL};
  */
  /* TODO: initialize the communication with the players */
  int pfd_parent_to_child[NUM_PLAYERS] [2];
  int pfd_child_to_parent[NUM_PLAYERS] [2];
  for (i = 0; i < NUM_PLAYERS; i++) {
    pipe(pfd_child_to_parent[i]);
    pipe(pfd_parent_to_child[i]);
  }

  for (i = 0; i < NUM_PLAYERS; i++) {
    /* TODO: spawn the processes that simulate the players */
    switch(pids[i] = fork()) {
    case -1:
      perror("could not create child process");
      exit(EXIT_FAILURE);
      break;
   
    case 0:	
      printf("PID of child: <%ld>", (long) getpid()); 
      
      shooter(i, pfd_parent_to_child[i][0], pfd_child_to_parent[i][1]);
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
       break;
     }
   }

  seed = time(NULL);
  for (i = 0; i < NUM_PLAYERS; i++) {
    seed++;
    char seedString[16] = "";

    sprintf(seedString, "%d", seed);
    write(pfd_parent_to_child[i][1], &seed, sizeof(seed));
  }


  /* TODO: get the dice results from the players, find the winner */
  int currentResult = 0;
  int childIndex = 0;
  int currentLeader = 0;
  int highestScore = 0;
  int pidResult;
  for (i = 0; i < NUM_PLAYERS; i++) {
    
    read(pfd_child_to_parent[i][0], &currentResult, sizeof(int));
    if(currentResult > highestScore) {
      currentLeader = childIndex;
      highestScore = currentResult;
    }
    childIndex++;
  }
  winner = currentLeader;
  printf("master: player %d WINS\n", winner);

  /* TODO: signal the winner */
  kill(pids[winner],SIGUSR1);
  /* TODO: signal all players the end of game */
  for (i = 0; i < NUM_PLAYERS; i++) {
    if(i != winner) {
      kill(pids[i], SIGUSR2);
      waitpid(pids[i], &pidResult, 0);
    }
  }

  printf("master: the game ends\n");

  /* TODO: cleanup resources and exit with success */
  for (i = 0; i < NUM_PLAYERS; i++) {
    close(pfd_parent_to_child[i][0]);
    close(pfd_parent_to_child[i][1]);
    close(pfd_child_to_parent[i][0]);
    close(pfd_child_to_parent[i][1]);
  }
  return 0;
}
