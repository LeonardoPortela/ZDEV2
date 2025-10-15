*----------------------------------------------------------------------*
***INCLUDE MZLES003COCKPIT9004 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9004 OUTPUT.
  SET PF-STATUS 'PF9004'.
  SET TITLEBAR  'TB9004'.

ENDMODULE.                 " STATUS_9004  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9004 INPUT.


  DATA: vld_dt_baixa TYPE sy-datum,
        wa_lotes_item_alv_aux TYPE zpfe_lote_item_alv,
        ok_atlz                TYPE c.

  IF ( sy-dynnr EQ '9004' ).

    CASE sy-ucomm.
      WHEN 'CANC' OR 'EXIT'  .
        LEAVE TO SCREEN 0.
      WHEN 'OK'.

        IF NOT ( wa_dt_baixa IS INITIAL ).

          CLEAR: vld_dt_baixa.
          vld_dt_baixa = sy-datum - 7.

          IF ( wa_dt_baixa < vld_dt_baixa ).
            MESSAGE w033.
          ELSE.

            PERFORM seleciona_item USING vg_selecionou.

            IF NOT ( vg_selecionou IS INITIAL ).

              LOOP AT it_lotes_item_sel INTO wa_lotes_item_alv_aux.

                UPDATE zpfe_lote_item
                  SET dt_baixa     = wa_dt_baixa
                WHERE nm_lote      EQ wa_lotes_item_alv_aux-nm_lote
                  AND nm_lote_item EQ wa_lotes_item_alv_aux-nm_lote_item.

                IF ( sy-subrc EQ 0 ).
                  COMMIT WORK.
                  ok_atlz = 'X'.
                ELSE.
                  ROLLBACK WORK.
                ENDIF.
              ENDLOOP.


              IF ( ok_atlz EQ 'X' ).
                MESSAGE s030 WITH 'Data de Baixa atualizada.'.
              ENDIF.

              LEAVE TO SCREEN 0.

            ENDIF.
          ENDIF.
        ELSE.

          MESSAGE w030 WITH 'Digite uma data!'.


        ENDIF.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_9004  INPUT
