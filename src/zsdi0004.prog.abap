*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0004                                                *
* Descrição  : Formação Preço Frame - Tabela                           *
* Módulo     : SD                                Transação: ZSDT0004   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 02/08/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdi0004 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TABLES: zsdt0020,
        kna1    .

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
DATA: BEGIN OF t_t0020 OCCURS 0.
INCLUDE TYPE zsdt0020.
DATA    marc TYPE char1.
DATA:  END  OF t_t0020.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_zsdt0020 LIKE TABLE OF t_t0020,
      t_sum_mes  LIKE TABLE OF t_t0020.

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA s_t0020 LIKE LINE OF t_t0020.

CONTROLS tc_t0020 TYPE TABLEVIEW USING SCREEN '0100'.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
SELECT-OPTIONS:
  s_status  FOR zsdt0020-status     NO-EXTENSION NO INTERVALS
                                    DEFAULT 01               ,
  s_kunnr   FOR zsdt0020-cliente    NO-EXTENSION NO INTERVALS,
  s_vbeln   FOR zsdt0020-vbeln      NO-EXTENSION NO INTERVALS,
  s_perio   FOR zsdt0020-periodo    NO-EXTENSION NO INTERVALS,
  s_ref     FOR zsdt0020-referencia NO-EXTENSION NO INTERVALS.
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

* Seleciona ZSDT0020
  PERFORM z_seleciona_zsdt0020.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0020                                     *
*&---------------------------------------------------------------------*
*                          Seleciona ZSDT0020                          *
*----------------------------------------------------------------------*
FORM z_seleciona_zsdt0020.

  SELECT *
    FROM zsdt0020
    INTO TABLE t_zsdt0020.

  t_t0020[] = t_zsdt0020[].
  DELETE t_t0020 WHERE: vbeln      NOT IN s_vbeln ,
                        status     NOT IN s_status,
                        cliente    NOT IN s_kunnr ,
                        periodo    NOT IN s_perio ,
                        referencia NOT IN s_ref   .

  IF t_t0020[] IS INITIAL.
    MESSAGE i836 WITH text-002.
  ENDIF.

  SORT t_zsdt0020 BY vbeln   ASCENDING
                     periodo DESCENDING.

  SORT t_t0020 BY vbeln   ASCENDING
                  periodo DESCENDING.

  t_sum_mes[] = t_t0020[].
  SORT t_sum_mes BY vbeln  ASCENDING
                   periodo ASCENDING.
  DELETE ADJACENT DUPLICATES FROM t_sum_mes COMPARING vbeln periodo.

ENDFORM.                    " Z_SELECIONA_ZSDT0020

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

  DESCRIBE TABLE t_t0020 LINES tc_t0020-lines.
  IF tc_t0020-lines EQ 0.
    tc_t0020-lines = 1.
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

  s_t0020-marc = 'X'.
  MODIFY t_t0020 FROM s_t0020
    TRANSPORTING marc
    WHERE marc IS INITIAL.

ENDFORM.                    " Z_MARC_ALL

*&---------------------------------------------------------------------*
*&      Form  Z_MARC_NONE                                              *
*&---------------------------------------------------------------------*
*                           Desmarca as Linhas                         *
*----------------------------------------------------------------------*
FORM z_marc_none.

  s_t0020-marc = space.
  MODIFY t_t0020 FROM s_t0020
    TRANSPORTING marc
    WHERE marc NE space.

ENDFORM.                    " Z_MARC_NONE

*&---------------------------------------------------------------------*
*&      Form  Z_DEL                                                    *
*&---------------------------------------------------------------------*
*                              Deleta Linhas                           *
*----------------------------------------------------------------------*
FORM z_del.

  DATA: tl_t0020  LIKE TABLE OF t_t0020,
        vl_answer TYPE char1           .

  tl_t0020[] = t_t0020[].
  DELETE tl_t0020 WHERE marc   EQ space
                     OR status EQ 2.

  IF tl_t0020[] IS INITIAL.
    MESSAGE i836 WITH text-007.
    EXIT.
  ENDIF.

  DELETE: tl_t0020 WHERE id   IS INITIAL,
          t_t0020  WHERE marc NE space
                     AND id   IS INITIAL.

  IF NOT tl_t0020[] IS INITIAL.
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
      DELETE zsdt0020 FROM TABLE tl_t0020.
      MESSAGE i836 WITH text-009.
      DELETE t_t0020 WHERE marc NE space AND
                         status NE 2.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_DEL

*&---------------------------------------------------------------------*
*&      Module  Z_VBELN  INPUT                                         *
*&---------------------------------------------------------------------*
*                             Campo VBELN                              *
*----------------------------------------------------------------------*
MODULE z_vbeln INPUT.

  CLEAR: s_t0020-cliente ,
         s_t0020-name1   ,
         s_t0020-qtd_cont,
         s_t0020-matnr   ,
         s_t0020-maktx   .

  IF NOT s_t0020-vbeln IS INITIAL.
*   Verifica Ordem
    PERFORM z_verifica_ord.
  ELSE.
    MODIFY t_t0020 FROM s_t0020 INDEX tc_t0020-current_line
      TRANSPORTING cliente qtd_cont.
  ENDIF.

ENDMODULE.                 " Z_VBELN  INPUT

*&---------------------------------------------------------------------*
*&      Module  Z_MARC  INPUT                                          *
*&---------------------------------------------------------------------*
*                             Marcação Linha                           *
*----------------------------------------------------------------------*
MODULE z_marc INPUT.

  MODIFY t_t0020 FROM s_t0020 INDEX tc_t0020-current_line
    TRANSPORTING marc.

ENDMODULE.                 " Z_MARC  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_ORD                                           *
*&---------------------------------------------------------------------*
*                             Verifica Ordem                           *
*----------------------------------------------------------------------*
FORM z_verifica_ord.

  SELECT SINGLE kunnr
    FROM vbak
    INTO s_t0020-cliente
  WHERE  vbeln EQ s_t0020-vbeln.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e836 WITH text-003.
  ENDIF.

  SELECT SINGLE matnr zmeng arktx
    FROM vbap
    INTO (s_t0020-matnr, s_t0020-qtd_cont, s_t0020-maktx)
  WHERE  vbeln EQ s_t0020-vbeln.

  SELECT SINGLE name1
    FROM kna1
    INTO s_t0020-name1
  WHERE  kunnr EQ s_t0020-cliente.

  MODIFY t_t0020 FROM s_t0020 INDEX tc_t0020-current_line
      TRANSPORTING vbeln cliente name1 qtd_cont matnr maktx.
  IF NOT sy-subrc IS INITIAL.
    APPEND s_t0020 TO t_t0020.
  ENDIF.

ENDFORM.                    " Z_VERIFICA_ORD

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
*&      Module  Z_QTD_MES  INPUT                                       *
*&---------------------------------------------------------------------*
*                          Quantidade Mês                              *
*----------------------------------------------------------------------*
MODULE z_qtd_mes INPUT.

  PERFORM z_verifica_qtd.

ENDMODULE.                 " Z_QTD_MES  INPUT
*&---------------------------------------------------------------------*
*&      Module  Z_ATUALIZA_CAMPOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_atualiza_campos INPUT.

  MODIFY t_t0020 FROM s_t0020 INDEX tc_t0020-current_line.

ENDMODULE.                 " Z_ATUALIZA_CAMPOS  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_TRATA_CAMPOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_trata_campos OUTPUT.

  LOOP AT SCREEN.

    CASE screen-group1.
      WHEN 'GR1'.
        IF s_t0020-vbeln IS INITIAL.
          screen-input  = 0.
          screen-output = 1.
        ELSE.
          screen-input  = 1.
          screen-output = 1.
        ENDIF.
    ENDCASE.

    CASE screen-group2.
      WHEN 'GR2'.
        IF s_t0020-status EQ 2.
          screen-input  = 0.
          screen-output = 1.
        ENDIF.
    ENDCASE.

    CASE screen-group3.
      WHEN 'GR3'.
        IF NOT s_t0020-id IS INITIAL.
          screen-input  = 0.
          screen-output = 1.
        ENDIF.
    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

ENDMODULE.                 " ZM_TRATA_CAMPOS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INS                                                    *
*&---------------------------------------------------------------------*
*                             Inseri Linhas                            *
*----------------------------------------------------------------------*
FORM z_ins.
  APPEND INITIAL LINE TO t_t0020.
ENDFORM.                    " Z_INS

*&---------------------------------------------------------------------*
*&      Form  Z_SAVE                                                   *
*&---------------------------------------------------------------------*
*                                Salva                                 *
*----------------------------------------------------------------------*
FORM z_save.

  DATA: tl_t0020 TYPE TABLE OF zsdt0020,
        tl_aux   TYPE TABLE OF zsdt0020,
        tl_aux2  TYPE TABLE OF zsdt0020,
        sl_t0020 TYPE zsdt0020         ,
        sl_zsdt0020 TYPE zsdt0020      ,
        vl_index TYPE i                ,
        vl_id    TYPE zid              .

  tl_t0020[] = t_t0020[].

  DELETE tl_t0020 WHERE status EQ 2.

  IF tl_t0020[] IS INITIAL.
    MESSAGE i836 WITH text-005.
    EXIT.
  ENDIF.

  LOOP AT tl_t0020 INTO sl_t0020.
    vl_index = sy-tabix.
    tl_aux2[] = tl_aux[].
    IF sl_t0020-id IS INITIAL.
      READ TABLE t_zsdt0020 INTO sl_zsdt0020
        WITH KEY vbeln = sl_t0020-vbeln
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        vl_id = sl_zsdt0020-id.
        SORT tl_aux2 BY vbeln ASCENDING
                           id DESCENDING.
        READ TABLE tl_aux2 INTO sl_zsdt0020
           WITH KEY vbeln = sl_t0020-vbeln
           BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          vl_id = sl_zsdt0020-id.
        ENDIF.
      ELSE.
        SORT tl_aux2 BY vbeln ASCENDING
                           id DESCENDING.
        READ TABLE tl_aux2 INTO sl_zsdt0020
           WITH KEY vbeln = sl_t0020-vbeln
           BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          vl_id = sl_zsdt0020-id.
        ENDIF.
      ENDIF.
      ADD 1 TO vl_id.
    ELSE.
      vl_id = sl_t0020-id.
    ENDIF.
    sl_t0020-status = 1.
    sl_t0020-id = vl_id.
    APPEND sl_t0020 TO tl_aux.
    CLEAR: sl_t0020   ,
           sl_zsdt0020,
           vl_id      .
  ENDLOOP.

  MODIFY zsdt0020 FROM TABLE tl_aux.
  MESSAGE i836 WITH text-006.
  LEAVE TO SCREEN 0.

ENDFORM.                    " Z_SAVE

*&---------------------------------------------------------------------*
*&      Form  Z_COPY                                                   *
*&---------------------------------------------------------------------*
*                                 Copiar                               *
*----------------------------------------------------------------------*
FORM z_copy.

  DATA: tl_t0020 LIKE TABLE OF t_t0020,
        sl_t0020 LIKE LINE OF t_t0020 .

  tl_t0020[] = t_t0020[].
  DELETE tl_t0020 WHERE marc IS INITIAL.

  IF tl_t0020[] IS INITIAL.
    MESSAGE i836 WITH text-010.
    EXIT.
  ENDIF.

  LOOP AT tl_t0020 INTO sl_t0020.

    sl_t0020-id              = space.
    sl_t0020-marc            = space.
    sl_t0020-qtd_mes         = space.
    sl_t0020-qtd_fix_premio  = space.
    sl_t0020-qtd_fix_chicago = space.
    sl_t0020-qtd             = space.
    APPEND sl_t0020 TO t_t0020.
    CLEAR sl_t0020.

  ENDLOOP.

ENDFORM.                    " Z_COPY

*&---------------------------------------------------------------------*
*&      Module  Z_QTD_PRE  INPUT                                       *
*&---------------------------------------------------------------------*
*                          Quantidade Prêmio                           *
*----------------------------------------------------------------------*
MODULE z_qtd_pre INPUT.

  PERFORM z_verifica_pre.

ENDMODULE.                 " Z_QTD_PRE  INPUT

*&---------------------------------------------------------------------*
*&      Module  Z_QTD_CHI  INPUT                                       *
*&---------------------------------------------------------------------*
*                          Quantidade Chicago                          *
*----------------------------------------------------------------------*
MODULE z_qtd_chi INPUT.

  PERFORM z_verifica_chi.

ENDMODULE.                 " Z_QTD_CHI  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_PRE                                           *
*&---------------------------------------------------------------------*
*                           Quantidade Prêmio                          *
*----------------------------------------------------------------------*
FORM z_verifica_pre.

  DATA: sl_t0020 LIKE LINE OF t_t0020,
        vl_qtd   TYPE zqtdfix        .

  CLEAR vl_qtd.

  LOOP AT t_t0020 INTO sl_t0020.
    IF sy-tabix EQ tc_t0020-current_line.
      CONTINUE.
    ENDIF.
    IF sl_t0020-vbeln NE s_t0020-vbeln.
      CONTINUE.
    ENDIF.
    IF sl_t0020-periodo NE s_t0020-periodo.
      CONTINUE.
    ENDIF.
    ADD sl_t0020-qtd_fix_premio TO vl_qtd.
    CLEAR sl_t0020.
  ENDLOOP.

  vl_qtd = vl_qtd + s_t0020-qtd_fix_premio.

  IF vl_qtd GT s_t0020-qtd_mes.
    MESSAGE e836 WITH text-011.
  ENDIF.

  MODIFY t_t0020 FROM s_t0020
    INDEX tc_t0020-current_line
    TRANSPORTING qtd_fix_premio.

ENDFORM.                    " Z_VERIFICA_PRE

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_CHI                                           *
*&---------------------------------------------------------------------*
*                          Quantidade Chicago                          *
*----------------------------------------------------------------------*
FORM z_verifica_chi.

  DATA: sl_t0020 LIKE LINE OF t_t0020,
          vl_qtd   TYPE zqtdfix        .

  CLEAR vl_qtd.

  LOOP AT t_t0020 INTO sl_t0020.
    IF sy-tabix EQ tc_t0020-current_line.
      CONTINUE.
    ENDIF.
    IF sl_t0020-vbeln NE s_t0020-vbeln.
      CONTINUE.
    ENDIF.
    IF sl_t0020-periodo NE s_t0020-periodo.
      CONTINUE.
    ENDIF.
    ADD sl_t0020-qtd_fix_chicago TO vl_qtd.
    CLEAR sl_t0020.
  ENDLOOP.

  vl_qtd = vl_qtd + s_t0020-qtd_fix_chicago.

  IF vl_qtd GT s_t0020-qtd_mes.
    MESSAGE e836 WITH text-011.
  ENDIF.

  MODIFY t_t0020 FROM s_t0020
    INDEX tc_t0020-current_line
    TRANSPORTING qtd_fix_chicago.

ENDFORM.                    " Z_VERIFICA_CHI

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_QTD                                           *
*&---------------------------------------------------------------------*
*                        Verifica Quantidade Mês                       *
*----------------------------------------------------------------------*
FORM z_verifica_qtd.

  DATA: sl_t0020 LIKE LINE OF t_t0020,
        vl_qtd   TYPE zqtdfix        .

  IF s_t0020-qtd_mes GT s_t0020-qtd_cont.
    MESSAGE e836 WITH text-004.
  ENDIF.

  CLEAR vl_qtd.

  READ TABLE t_sum_mes
    WITH KEY vbeln   = s_t0020-vbeln
             periodo = s_t0020-periodo
    BINARY SEARCH
    TRANSPORTING NO FIELDS.

*  LOOP AT t_t0020 INTO sl_t0020.
*    IF sy-tabix EQ tc_t0020-current_line.
*      CONTINUE.
*    ENDIF.
*    IF sl_t0020-vbeln NE s_t0020-vbeln.
*      CONTINUE.
*    ENDIF.
*    ADD sl_t0020-qtd_mes TO vl_qtd.
*    CLEAR sl_t0020.
*  ENDLOOP.
*
*  vl_qtd = vl_qtd + s_t0020-qtd_mes.

  IF NOT sy-subrc IS INITIAL.
    LOOP AT t_sum_mes INTO sl_t0020.
      ADD sl_t0020-qtd_mes TO vl_qtd.
      CLEAR sl_t0020.
    ENDLOOP.
  ENDIF.

  vl_qtd = vl_qtd + s_t0020-qtd_mes.

  IF vl_qtd GT s_t0020-qtd_cont.
    MESSAGE e836 WITH text-004.
  ENDIF.

  MODIFY t_t0020 FROM s_t0020
    INDEX tc_t0020-current_line
    TRANSPORTING qtd_mes.

ENDFORM.                    " Z_VERIFICA_QTD

*&---------------------------------------------------------------------*
*&      Module  Z_QTD  INPUT                                           *
*&---------------------------------------------------------------------*
*                                Quantidade                            *
*----------------------------------------------------------------------*
MODULE z_qtd INPUT.

  PERFORM z_verifica_qtd_2.

ENDMODULE.                 " Z_QTD  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_QTD_2                                         *
*&---------------------------------------------------------------------*
*                                Quantidade                            *
*----------------------------------------------------------------------*
FORM z_verifica_qtd_2.

  IF s_t0020-qtd GT s_t0020-qtd_cont.
    MESSAGE e836 WITH text-013.
  ENDIF.

*  IF NOT s_t0020-qtd IS INITIAL.
*    MESSAGE e836 WITH text-014.
*  ENDIF.

  MODIFY t_t0020 FROM s_t0020
    INDEX tc_t0020-current_line
    TRANSPORTING qtd.

ENDFORM.                    " Z_VERIFICA_QTD_2
