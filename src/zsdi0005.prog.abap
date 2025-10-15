*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0005                                                *
* Descrição  : Deleção Memorandos                                      *
* Módulo     : SD                                Transação: ZSDT0007   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 10/08/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdi0005 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TABLES zsdt0004.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
DATA: BEGIN OF t_t0004 OCCURS 0.
INCLUDE TYPE zsdt0004.
DATA    marc TYPE char1.
DATA:  END  OF t_t0004.
*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA s_t0004 LIKE LINE OF t_t0004.

CONTROLS tc_t0004 TYPE TABLEVIEW USING SCREEN '0100'.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
SELECT-OPTIONS
  s_numme FOR zsdt0004-numme NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END   OF BLOCK a2.
SELECTION-SCREEN END   OF BLOCK a1.

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Seleção Dados
  PERFORM: z_seleciona_dados.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                            Seleção Dados                             *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.

* Seleciona ZSDT0004
  PERFORM z_seleciona_zsdt0004.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0004                                     *
*&---------------------------------------------------------------------*
*                            Seleciona ZSDT0004                        *
*----------------------------------------------------------------------*
FORM z_seleciona_zsdt0004.

  REFRESH t_t0004.

  SELECT *
    FROM zsdt0004
    INTO TABLE t_t0004
  WHERE  numme IN s_numme.

  DELETE t_t0004 WHERE numme IS INITIAL.

  IF t_t0004[] IS INITIAL.
    MESSAGE i836 WITH text-002.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_ZSDT0004

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
    WHEN OTHERS.
  ENDCASE.

  DESCRIBE TABLE t_t0004 LINES tc_t0004-lines.
  IF tc_t0004-lines EQ 0.
    tc_t0004-lines = 1.
  ENDIF.
ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  Z_MARC  INPUT                                          *
*&---------------------------------------------------------------------*
*                             Marcação Linha                           *
*----------------------------------------------------------------------*
MODULE z_marc INPUT.

  MODIFY t_t0004 FROM s_t0004 INDEX tc_t0004-current_line
    TRANSPORTING marc.

ENDMODULE.                 " Z_MARC  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CASE sy-ucomm.
    WHEN 'BT_ALL'.
*     Marca Todas as Linhas
      PERFORM z_marc_all.
    WHEN 'BT_NONE'.
*     Desmarca as Linhas
      PERFORM z_marc_none.
    WHEN 'BT_DEL'.
*     Deleta Linhas
      PERFORM z_del.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_MARC_ALL                                               *
*&---------------------------------------------------------------------*
*                          Marca Todas as Linhas                       *
*----------------------------------------------------------------------*
FORM z_marc_all.

  s_t0004-marc = 'X'.
  MODIFY t_t0004 FROM s_t0004
    TRANSPORTING marc
    WHERE marc IS INITIAL.

ENDFORM.                    " Z_MARC_ALL

*&---------------------------------------------------------------------*
*&      Form  Z_MARC_NONE                                              *
*&---------------------------------------------------------------------*
*                           Desmarca as Linhas                         *
*----------------------------------------------------------------------*
FORM z_marc_none.

  s_t0004-marc = space.
  MODIFY t_t0004 FROM s_t0004
    TRANSPORTING marc
    WHERE marc NE space.

ENDFORM.                    " Z_MARC_NONE

*&---------------------------------------------------------------------*
*&      Form  Z_DEL                                                    *
*&---------------------------------------------------------------------*
*                              Deleta Linhas                           *
*----------------------------------------------------------------------*
FORM z_del.

  DATA: tl_t0004  LIKE TABLE OF t_t0004,
        vl_answer TYPE char1           .

  tl_t0004[] = t_t0004[].
  DELETE tl_t0004 WHERE marc EQ space.

  IF tl_t0004[] IS INITIAL.
    MESSAGE i836 WITH text-003.
    EXIT.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = text-005
    IMPORTING
      answer         = vl_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  IF vl_answer EQ '1'.
    DELETE zsdt0004 FROM TABLE tl_t0004.
    MESSAGE i836 WITH text-004.
    DELETE t_t0004 WHERE marc NE space.
  ENDIF.

ENDFORM.                    " Z_DEL
