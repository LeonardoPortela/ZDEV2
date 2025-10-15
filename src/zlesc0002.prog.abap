*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZLESC0002                                               *
* Descrição  : Controle de Impressão Carta Frete                       *
* Módulo     : LES                               Transação:            *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 06/10/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zlesc0002 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TABLES: zlest0014,
        kna1     .

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
DATA: BEGIN OF t_t0014 OCCURS 0.
INCLUDE TYPE zlest0014.
DATA    marc TYPE char1.
DATA:  END  OF t_t0014.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA t_zlest0014 LIKE TABLE OF t_t0014.

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA s_t0014 LIKE LINE OF t_t0014.

CONTROLS tc_t0014 TYPE TABLEVIEW USING SCREEN '0100'.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
" Comentado por Camila Brand conforme solicitação 24.05.2011
*SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
*SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
*SELECT-OPTIONS:
*  s_conhec FOR zlest0014-conhec.
*SELECTION-SCREEN END   OF BLOCK a2.
*SELECTION-SCREEN END   OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
 p_tknum  FOR zlest0014-tknum NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK a1.

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Seleção Dados
  PERFORM z_seleciona_dados.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                            Seleção Dados                             *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.

* Seleciona ZLEST0014
  PERFORM z_seleciona_zlest0014.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZLEST0014                                     *
*&---------------------------------------------------------------------*
*                          Seleciona ZLEST0014                          *
*----------------------------------------------------------------------*
FORM z_seleciona_zlest0014.
  " Comentado por Camila Brand conforme solicitação
*  SELECT *
*    FROM zlest0014
*    INTO TABLE t_zlest0014
*  WHERE  conhec IN s_conhec.

  IF p_tknum IS INITIAL.
    MESSAGE i836 WITH text-012.
  ELSE.
    SELECT *
    FROM zlest0014
    INTO TABLE t_zlest0014
  WHERE tknum    IN p_tknum.

    t_t0014[] = t_zlest0014[].

    IF t_t0014[] IS INITIAL.
      MESSAGE i836 WITH text-002.
      LEAVE LIST-PROCESSING.
    ENDIF.

    SORT: t_zlest0014 BY tknum ASCENDING,
          t_t0014     BY tknum ASCENDING.

    CALL SCREEN 0100.

  ENDIF.
ENDFORM.                    " Z_SELECIONA_ZLEST0014

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR 'PF0100'.
    WHEN OTHERS.
  ENDCASE.

  DESCRIBE TABLE t_t0014 LINES tc_t0014-lines.
  IF tc_t0014-lines EQ 0.
    tc_t0014-lines = 1.
  ENDIF.

ENDMODULE.                 " ZM_STATUS  OUTPUT

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
    WHEN 'BT_INS'.
*     Inseri Linhas
      PERFORM z_ins.
    WHEN 'BT_DEL'.
*     Deleta Linhas
      PERFORM z_del.
    WHEN 'SAVE'.
*     Salva
      PERFORM z_save.
    WHEN 'BT_COPY'.
*     Copiar
      PERFORM z_copy.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_MARC_ALL                                               *
*&---------------------------------------------------------------------*
*                          Marca Todas as Linhas                       *
*----------------------------------------------------------------------*
FORM z_marc_all.

  s_t0014-marc = 'X'.
  MODIFY t_t0014 FROM s_t0014
    TRANSPORTING marc
    WHERE marc IS INITIAL.

ENDFORM.                    " Z_MARC_ALL

*&---------------------------------------------------------------------*
*&      Form  Z_MARC_NONE                                              *
*&---------------------------------------------------------------------*
*                           Desmarca as Linhas                         *
*----------------------------------------------------------------------*
FORM z_marc_none.

  s_t0014-marc = space.
  MODIFY t_t0014 FROM s_t0014
    TRANSPORTING marc
    WHERE marc NE space.

ENDFORM.                    " Z_MARC_NONE

*&---------------------------------------------------------------------*
*&      Form  Z_DEL                                                    *
*&---------------------------------------------------------------------*
*                              Deleta Linhas                           *
*----------------------------------------------------------------------*
FORM z_del.

  DATA: tl_t0014 LIKE TABLE OF t_t0014,
        vl_answer TYPE char1           .

  tl_t0014[] = t_t0014[].
  DELETE tl_t0014 WHERE marc EQ space.

  IF tl_t0014[] IS INITIAL.
    MESSAGE i836 WITH text-007.
    EXIT.
  ENDIF.

  IF NOT tl_t0014[] IS INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = text-008
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
      DELETE zlest0014 FROM TABLE tl_t0014.
      MESSAGE i836 WITH text-009.
      DELETE t_t0014 WHERE marc NE space.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_DEL

*&---------------------------------------------------------------------*
*&      Module  Z_MARC  INPUT                                          *
*&---------------------------------------------------------------------*
*                             Marcação Linha                           *
*----------------------------------------------------------------------*
MODULE z_marc INPUT.

  MODIFY t_t0014 FROM s_t0014 INDEX tc_t0014-current_line
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
*&      Module  Z_ATUALIZA_CAMPOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_atualiza_campos INPUT.

  s_t0014-data    = sy-datum.
  s_t0014-hora    = sy-uzeit.
  s_t0014-usuario = sy-uname.
  MODIFY t_t0014 FROM s_t0014 INDEX tc_t0014-current_line.

  IF s_t0014-motivo IS INITIAL AND ( ( s_t0014-reimp EQ 'A' ) OR ( s_t0014-alterar EQ 'A' ) ).
    MESSAGE e836 WITH text-011.
  ENDIF.

ENDMODULE.                 " Z_ATUALIZA_CAMPOS  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_TRATA_CAMPOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_trata_campos OUTPUT.

  DATA: vg_ctafrete TYPE zlest0014-ctafrete.

  SELECT SINGLE ctafrete INTO vg_ctafrete
  FROM zlest0014
  WHERE tknum    =  s_t0014-tknum
  AND  conhec    =  s_t0014-conhec
  AND  actafrete =  s_t0014-ctafrete.

  IF sy-subrc IS INITIAL.

    LOOP AT SCREEN.
      IF screen-name EQ 'S_T0014-REIMP' OR screen-name EQ 'S_T0014-ALTERAR' OR screen-name EQ 'S_T0014-MOTIVO'.
        screen-input  = 0.
        screen-output = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " ZM_TRATA_CAMPOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INS                                                    *
*&---------------------------------------------------------------------*
*                             Inseri Linhas                            *
*----------------------------------------------------------------------*
FORM z_ins.
  APPEND INITIAL LINE TO t_t0014.
ENDFORM.                    " Z_INS

*&---------------------------------------------------------------------*
*&      Form  Z_SAVE                                                   *
*&---------------------------------------------------------------------*
*                                Salva                                 *
*----------------------------------------------------------------------*
FORM z_save.

*  DATA: sl_t0014 LIKE LINE OF t_t0014,
*        vl_index TYPE i              .
*
*  LOOP AT t_t0014 INTO sl_t0014.
*
*    vl_index = sy-tabix.
*
*    IF sl_t0014-usuario IS INITIAL.
*      sl_t0014-data    = sy-datum.
*      sl_t0014-hora    = sy-uzeit.
*      sl_t0014-usuario = sy-uname.
*      MODIFY t_t0014 FROM sl_t0014 INDEX vl_index
*        TRANSPORTING data
*                     hora
*                     usuario.
*    ENDIF.
*
*    CLEAR: vl_index,
*           sl_t0014.
*
*  ENDLOOP.

  MODIFY zlest0014 FROM TABLE t_t0014.
  MESSAGE i836 WITH text-006.
  LEAVE TO SCREEN 0.


ENDFORM.                    " Z_SAVE

*&---------------------------------------------------------------------*
*&      Form  Z_COPY                                                   *
*&---------------------------------------------------------------------*
*                                 Copiar                               *
*----------------------------------------------------------------------*
FORM z_copy.

  DATA: tl_t0014 LIKE TABLE OF t_t0014,
        sl_t0014 LIKE LINE OF t_t0014 .

  tl_t0014[] = t_t0014[].
  DELETE tl_t0014 WHERE marc IS INITIAL.

  IF tl_t0014[] IS INITIAL.
    MESSAGE i836 WITH text-010.
    EXIT.
  ENDIF.

  LOOP AT tl_t0014 INTO sl_t0014.
    sl_t0014-marc    = space.
    sl_t0014-tknum   = space.
    sl_t0014-data    = sy-datum.
    sl_t0014-hora    = sy-uzeit.
    sl_t0014-usuario = sy-uname.
    APPEND sl_t0014 TO t_t0014.
    CLEAR sl_t0014.
  ENDLOOP.

ENDFORM.                    " Z_COPY

*&---------------------------------------------------------------------*
*&      Module  Z_TKNUM  INPUT                                         *
*&---------------------------------------------------------------------*
*                         Verifica Transporte                          *
*----------------------------------------------------------------------*
MODULE z_tknum INPUT.

  CHECK NOT s_t0014-tknum IS INITIAL.

  SELECT SINGLE tknum
    FROM vttk
    INTO s_t0014-tknum
  WHERE  tknum EQ s_t0014-tknum.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e836 WITH text-003.
  ENDIF.

  SELECT SINGLE tknum
    FROM zlest0014
    INTO s_t0014-tknum
  WHERE  tknum EQ s_t0014-tknum.

  IF sy-subrc IS INITIAL.
    MESSAGE e836 WITH text-004.
  ENDIF.

  MODIFY t_t0014 FROM s_t0014 INDEX tc_t0014-current_line
    TRANSPORTING tknum.

ENDMODULE.                 " Z_TKNUM  INPUT
