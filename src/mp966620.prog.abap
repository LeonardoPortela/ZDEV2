* OUTPUT modules

*---------------------------------------------------------------------*
*       MODULE INIT_9666                                              *
*---------------------------------------------------------------------*
*       infotype specific initializations                             *
*---------------------------------------------------------------------*
MODULE init_9666 OUTPUT.
* replace with infotype specific coding
*    p9666-zvalor.
*    p9666-zperc.

ENDMODULE.                    "INIT_9666 OUTPUT

* INPUT modules
*&---------------------------------------------------------------------*
*&      Module  CHECK_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE check_data INPUT.
**  IF p9666-ztpbenef EQ 'GR'.
**    CLEAR p9666-zvalor.
**  ELSE.
**    CLEAR p9666-zperc.
**  ENDIF.
*  IF p9666-ztpbenef IS INITIAL.
*    LOOP AT SCREEN.
*      IF screen-name EQ '*P9666-ZVALOR'.
*        screen-input = 0.
*        screen-invisible = 0.
*        CLEAR p9666-zperc.
*      ENDIF.
*      IF screen-name EQ '*P9666-ZPERC'.
*        screen-input = 0.
*        screen-invisible = 0.
*        CLEAR p9666-zvalor.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDLOOP.
*  ELSE.
*    CASE p9666-ztpbenef.
*      WHEN 'GR'.
*        LOOP AT SCREEN.
*          IF screen-name EQ '*P9666-ZVALOR'.
*            screen-input = 0.
*            screen-invisible = 0.
*            CLEAR p9666-zvalor.
*            MODIFY SCREEN.
*          ENDIF.
*          IF screen-name EQ 'P9666-ZVALOR'.
*            screen-input = 0.
*            screen-invisible = 0.
*            CLEAR p9666-zvalor.
*            MODIFY SCREEN.
*          ENDIF.
*        ENDLOOP.
*      WHEN 'PR'.
*        LOOP AT SCREEN.
*          IF screen-name EQ '*P9666-ZPERC'.
*            screen-input = 0.
*            screen-invisible = 0.
*            CLEAR p9666-zperc.
*            MODIFY SCREEN.
*          ENDIF.
*          IF screen-name EQ 'P9666-ZPERC'.
*            screen-input = 0.
*            screen-invisible = 0.
*            MODIFY SCREEN.
*            CLEAR p9666-zperc.
*          ENDIF.
*        ENDLOOP.
*      WHEN 'PM'.
*        LOOP AT SCREEN.
*          IF screen-name EQ '*P9666-ZPERC'.
*            screen-input = 0.
*            screen-invisible = 0.
*            CLEAR p9666-zperc.
*            MODIFY SCREEN.
*
*          ENDIF.
*          IF screen-name EQ 'P9666-ZPERC'.
*            screen-input = 0.
*            screen-invisible = 0.
*            CLEAR p9666-zperc.
*            MODIFY SCREEN.
*          ENDIF.
*        ENDLOOP.
*      WHEN OTHERS.
*    ENDCASE.
*  ENDIF.
*ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VERIFICA_TIPO_BENEFICIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE verifica_tipo_beneficio INPUT.
  CASE OK-CODE.
    WHEN 'UPD'.
      CASE p9666-ztpbenef.
        WHEN 'GR'.
          IF p9666-zvalor IS NOT INITIAL.
            CLEAR p9666-zvalor.
            MESSAGE s301 WITH
            'incluir valor premio quando é selecionado Gratificação, o valor premio será zerado.'
            DISPLAY LIKE 'W'.
          ENDIF.
        WHEN 'PR'.
          IF p9666-zperc IS NOT INITIAL.
              CLEAR p9666-zperc.
              MESSAGE s301 WITH
              'incluir porcentagem quando é selecionado Premio, a porcentagem será zerada zerado.'
              DISPLAY LIKE 'W'.
          ENDIF.

        WHEN 'PM'.
          IF p9666-zperc IS NOT INITIAL.
              CLEAR p9666-zperc.
              MESSAGE s301 WITH
              'incluir porcentagem quando é selecionado Premio, a porcentagem será zerada zerado.'
              DISPLAY LIKE 'W'.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
  ENDCASE.

ENDMODULE.
