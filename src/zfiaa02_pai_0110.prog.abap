*&---------------------------------------------------------------------*
*&  Include           ZFIAA02_PAI_0110
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PAI_0110  INPUT
*&---------------------------------------------------------------------*
module pai_0110 input.
  create object: r_seleciona_dados,
                 r_utils.

  case sy-ucomm.
    when c_enter.
      data: return type c.
      clear return.

      if ( s_bukrs-low is initial ).
        clear gt_saida_0110.
        message s836(sd) with text-e03 display like 'E'.

      else.
*        IF ( S_WERKS IS NOT INITIAL ).
*          SELECT SINGLE *
*            FROM J_1BBRANCH
*            INTO WL_J_1BBRANCH
*           WHERE BUKRS  EQ S_BUKRS-LOW
*             AND BRANCH EQ S_WERKS-LOW.
*
*          IF SY-SUBRC IS NOT INITIAL.
*            CLEAR GT_SAIDA_0110.
*            MESSAGE S836(SD) WITH TEXT-E05 DISPLAY LIKE 'E'.
*            RETURN = 'X'.
*          ENDIF.
*        ENDIF.
*
*        CHECK RETURN IS INITIAL.
        r_seleciona_dados->seleciona_dados_0110( ).
      endif.
    when 'CANC'.
      leave program.
    when others.
  endcase.
endmodule.                 " PAI_0110  INPUT
