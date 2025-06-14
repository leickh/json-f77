      
      MODULE json_utility

      CONTAINS
        FUNCTION new_arena(capacity) RESULT(arena)
          INTEGER capacity
          
          BYTE,ALLOCATABLE :: arena(:)
          INTEGER(8) allocated_capacity
          allocated_capacity = capacity + 64

          ALLOCATE(arena(allocated_capacity))

          RETURN
        END FUNCTION new_arena

      END MODULE json_utility

