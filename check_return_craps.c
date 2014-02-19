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
    case 0: // fork returns 0 to the child if successful
      //printf("CHILD: My PID is: <%ld>\n", (long) getpid());

      if(close(pfd_parent_to_child[i][1]) == -1) { perror("Close fail...\n"); exit(EXIT_FAILURE); }
      if(close(pfd_child_to_parent[i][0]) == -1) { perror("Close fail...\n"); exit(EXIT_FAILURE); }
      for(int descIndex = 0; descIndex < NUM_PLAYERS; descIndex++) {
	if (descIndex != i) {
	  if(close(pfd_parent_to_child[descIndex][0]) == -1) { perror("Close fail...\n"); exit(EXIT_FAILURE); } 
	  if(close(pfd_parent_to_child[descIndex][1]) == -1) { perror("Close fail...\n"); exit(EXIT_FAILURE); }
	  if(close(pfd_child_to_parent[descIndex][0]) == -1) { perror("Close fail...\n"); exit(EXIT_FAILURE); }
	  if(close(pfd_child_to_parent[descIndex][1]) == -1) { perror("Close fail...\n"); exit(EXIT_FAILURE); }
	} 
      }

      //shooter(i, pfd_parent_to_child[i][0], pfd_child_to_parent[i][1]);
      
      if(dup2(pfd_parent_to_child[i][0], STDIN_FILENO) == -1) { perror("Dup2 fail...\n"); exit(EXIT_FAILURE); }
      if(close(pfd_parent_to_child[i][0]) == -1) { perror("Close fail...\n"); exit(EXIT_FAILURE); }

      if(dup2(pfd_child_to_parent[i][1], STDOUT_FILENO) == -1) { perror("Dup2 fail...\n"); exit(EXIT_FAILURE); }
      if(close(pfd_child_to_parent[i][1]) == -1) { perror("Close fail...\n"); exit(EXIT_FAILURE); }
      
      if(sprintf(arg1, "%d", i) < 0) { perror("Sprintf fail...\n"); exit(EXIT_FAILURE); }
      if(execv(args[0], args) == -1) { perror("Execv fail...\n"); exit(EXIT_FAILURE); }
      
      break;
    } // fork return the child PID to the parent process, pids[i] contains them
  }

  if((seed = time(NULL)) == -1) { perror("Time fail...\n"); exit(EXIT_FAILURE); }
  for (i = 0; i < NUM_PLAYERS; i++) {
    seed++;
    /* TODO: send the seed to the players */
    if(write(pfd_parent_to_child[i][1], &seed, sizeof(int)) == -1) { perror("Write fail...\n"); exit(EXIT_FAILURE); }
  }

  /* TODO: get the dice results from the players, find the winner */
  int highestScore = 0;
  for (i = 0; i < NUM_PLAYERS; i++) {
    int currentResult = 0;
    if(read(pfd_child_to_parent[i][0], &currentResult, sizeof(int)) == -1) { perror("Read fail...\n"); exit(EXIT_FAILURE); }
    if(currentResult > highestScore) {
      winner = i;
      highestScore = currentResult;
    }
  }

  printf("master: player %d WINS with a score of: %d\n", winner, highestScore);

  /* TODO: signal the winner */
  if(kill(pids[winner], SIGUSR1) == -1) { perror("Kill fail...\n"); exit(EXIT_FAILURE); }
  
  /* TODO: signal all players the end of game */
  int pidResult;
  for (i = 0; i < NUM_PLAYERS; i++) {
    if(kill(pids[i], SIGUSR2) == -1) { perror("Kill fail...\n"); exit(EXIT_FAILURE); }
    //wait(NULL);
    if(waitpid(pids[i], &pidResult, 0) == -1) { perror("Waitpid fail...\n"); exit(EXIT_FAILURE); }
    waitstat(pids[i], pidResult);
  }

  printf("master: the game ends\n");

  /* TODO: cleanup resources and exit with success */
  for (i = 0; i < NUM_PLAYERS; i++) {
    if(close(pfd_parent_to_child[i][0]) == -1) { perror("Close fail...\n"); exit(EXIT_FAILURE); }
    if(close(pfd_parent_to_child[i][1]) == -1) { perror("Close fail...\n"); exit(EXIT_FAILURE); }
    if(close(pfd_child_to_parent[i][0]) == -1) { perror("Close fail...\n"); exit(EXIT_FAILURE); }
    if(close(pfd_child_to_parent[i][1]) == -1) { perror("Close fail...\n"); exit(EXIT_FAILURE); }
  }

  exit(EXIT_SUCCESS);
  //return 0;
}
