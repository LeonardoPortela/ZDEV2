"Name: \PR:SAPLSOE2\FO:GET_SENDER\SE:BEGIN\EI
ENHANCEMENT 0 Z_SET_NOREPLY_1.

* Alterar ordem de envio inserindo usuário JOBADM como remetente
*  SELECT SINGLE * FROM SOOS
*    INTO @DATA(W_SOOS)
*    WHERE OBJTP = @I_SOSC-OBJTP AND
*          OBJYR = @I_SOSC-OBJYR AND
*          OBJNO = @I_SOSC-OBJNO AND
*          RECTP = @I_SOSC-RECTP AND
*          RECYR = @I_SOSC-RECYR AND
*          RECNO = @I_SOSC-RECNO.
*    IF ( SY-SUBRC = 0 ).
*
*      SELECT SINGLE SNDNO, SNDNAM
*        FROM SOOS
*        INTO @DATA(W_USER)
*        WHERE SNDNAM = 'JOBADM'.
*      IF ( SY-SUBRC = 0 ).
*        W_SOOS-SNDNO = W_USER-SNDNO.
*        W_SOOS-SNDNAM = W_USER-SNDNAM.
*      ENDIF.
*
*      DATA(V_REPEAT_ENQUEUE) = 'X'.
*      DATA(V_COUNTER_ENQUEUE) = 0.
*
*      WHILE V_REPEAT_ENQUEUE EQ 'X'.
*        CALL FUNCTION 'ENQUEUE_ESSOOS'
*          EXPORTING
*            OBJTP          = W_SOOS-OBJTP
*            OBJYR          = W_SOOS-OBJYR
*            OBJNO          = W_SOOS-OBJNO
*            FORTP          = W_SOOS-FORTP
*            FORYR          = W_SOOS-FORYR
*            FORNO          = W_SOOS-FORNO
*            RECTP          = W_SOOS-RECTP
*            RECYR          = W_SOOS-RECYR
*            RECNO          = W_SOOS-RECNO
*            _WAIT          = V_REPEAT_ENQUEUE
*          EXCEPTIONS
*            FOREIGN_LOCK   = 1
*            SYSTEM_FAILURE = 2
*            OTHERS         = 3.
*
*        IF SY-SUBRC = 0.
*
*          V_REPEAT_ENQUEUE = ''.
*
*          MODIFY SOOS FROM W_SOOS.
*
*          COMMIT WORK.
*
*          CALL FUNCTION 'DEQUEUE_ESSOOS'
*          EXPORTING
*            OBJTP          = W_SOOS-OBJTP
*            OBJYR          = W_SOOS-OBJYR
*            OBJNO          = W_SOOS-OBJNO
*            FORTP          = W_SOOS-FORTP
*            FORYR          = W_SOOS-FORYR
*            FORNO          = W_SOOS-FORNO
*            RECTP          = W_SOOS-RECTP
*            RECYR          = W_SOOS-RECYR
*            RECNO          = W_SOOS-RECNO.
*
*        ELSE.
*
*          ADD 1 TO V_COUNTER_ENQUEUE.
*          IF V_REPEAT_ENQUEUE > 50.
*            V_REPEAT_ENQUEUE = ''.
*          ENDIF.
*        ENDIF.
*
*      ENDWHILE.
*
*    ENDIF.

ENDENHANCEMENT.
