      
      MODULE json_utility
          IMPLICIT NONE

      CONTAINS
        FUNCTION new_arena(capacity) RESULT(arena)
          IMPLICIT NONE

          INTEGER(4) :: capacity
          INTEGER(8) :: allocated_capacity

          INTEGER(1),DIMENSION(:),POINTER :: arena

          allocated_capacity = 0
          allocated_capacity = capacity + 64

          ALLOCATE(arena(allocated_capacity))

C       Write the capacity byte-by-byte
          arena(1) = MOD(capacity, 256)
          arena(2) = MOD(capacity, 256*256) - arena(1)
          arena(3) = MOD(capacity, 256*256*256) -  arena(1) - arena(2)
          arena(4) = MOD(capacity, (256*256*256*128)) - arena(1) -      &
     &      arena(2) - arena(3)

C       Set the usage to zero byte-by-byte
          arena(5) = 0
          arena(6) = 0
          arena(7) = 0
          arena(8) = 0
          RETURN
        END FUNCTION new_arena
        
        FUNCTION arena_alloc(arena, bytes) RESULT(memory_block)
          IMPLICIT NONE
          
          INTEGER(1),DIMENSION(:),POINTER :: arena
          INTEGER(4) :: bytes
          INTEGER(1),DIMENSION(:),POINTER :: memory_block

C         @todo
        END FUNCTION arena_alloc

      END MODULE json_utility

