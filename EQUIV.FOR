 
      LOGICAL FUNCTION equiv(q,r)
      LOGICAL q , r
      equiv = .FALSE.
      IF ( q .AND. r ) equiv = .TRUE.
      IF ( .NOT.q .AND. .NOT.r ) equiv = .TRUE.
C
      RETURN
      END
