C * Subroutine FILINT fills an integer array with a value passed from the
C   calling routine.
 
      SUBROUTINE filint(name,length,value,begin)
 
C# Name = the name of the array to be filled.
      INTEGER name(*)
C# Length = the number of elements in the array to be filled (default=1).
      INTEGER length
C# Value = the array element value (default=0).
      INTEGER value
C# Begin = the first position to fill of the string passed (default=1).
      INTEGER begin
 
C# Index/counter variables for looping.
      INTEGER i , j , k
 
C * Get first element to change and length of array.
 
      j = begin
      IF ( j.LE.0 ) j = 1
      k = length
      IF ( k.LE.0 ) k = 1
 
C * Change element Begin to Length.
 
      DO i = j , k
         name(i) = value
      ENDDO
 
      RETURN
      END
