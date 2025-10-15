*&---------------------------------------------------------------------*
*&  Include           ZLESR0162_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'LEAVE' OR
         'BACK' OR
         'EXIT'.

      SET PARAMETER ID 'COD_CLI' FIELD space.
      SET PARAMETER ID 'COD_PC'  FIELD space.
      SET PARAMETER ID 'ZONA_DES'  FIELD space.
      SET PARAMETER ID 'ZONA_ORI'  FIELD space.


      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'EXEC'.

      PERFORM verifica_zona. "145190-SMC
      PERFORM f_create_job.

    WHEN 'ALTERA_ZONA_LR'.

      IF ( wa_kna1-name1 IS NOT INITIAL ).
        PERFORM altera_zona_lr.
      ENDIF.

    WHEN 'ALTERA_ZONA_PC'.

      IF ( wa_lfa1_pc-name1 IS NOT INITIAL ).
        PERFORM altera_zona_pc.
      ENDIF.

    WHEN OTHERS.

      PERFORM f_preenche_campos.

  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_FORMA_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_forma_lote INPUT.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form verifica_zona
*&---------------------------------------------------------------------*
*& text: 145190-ZLES0214  -  Validar zona de transporte- SMC
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM verifica_zona .

 select * from TZONE
   where zone1 = ''
    INTO TABLE @DATA(IT_TZONE).


  IF it_tzone IS NOT INITIAL.
  LOOP AT IT_TZONE ASSIGNING FIELD-SYMBOL(<_DELL>).
      DELETE TZONE FROM <_DELL>.
      COMMIT WORK.
  ENDLOOP.

    ENDIF.


ENDFORM.
