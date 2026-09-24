#' Insertion Sort Algorithm
#' This function implements the insertion sort algorithm to sort a given array of numbers in ascending order.
#' The insertion sort algorithm works by dividing the array into a sorted and an unsorted part. It iteratively takes elements from the unsorted part and inserts them into the correct position in the sorted part until the entire array is sorted.
#' The time complexity of the insertion sort algorithm is O(n^2) in the worst case, which occurs when the input array is sorted in reverse order. However, it performs well on small or partially sorted arrays, with a best-case time complexity of O(n) when the input array is already sorted.
#' The space complexity of the insertion sort algorithm is O(1) because it only requires a constant amount of additional space for the key variable and the loop counters.
#' 
#' @param A A numeric vector that you want to sort.
#' @param n The length of the vector A.
#' @return A sorted numeric vector in ascending order.
#' 
#' @examples
#' A <- c(5, 2, 4, 6, 1, 3)
#' insertion_sort(A, length(A))
insertion_sort <- function(A, n){
  for (i in 2:n){
    key = A[i] # next position of the for loop
    j = i - 1 # first number in the array
    while (j > 0 && A[j] > key){
      A[j + 1] = A[j] # move the previous number one position to the right
      j = j - 1 # move the pointer to the left
    }
    A[j + 1] = key # insert the key in the correct position
  }
  print(A)
}

