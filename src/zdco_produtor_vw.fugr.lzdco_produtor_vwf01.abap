*----------------------------------------------------------------------*
***INCLUDE LZDCO_PRODUTOR_VWF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  Z_SHOW_MATCH_FORNECEDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_show_match_fornecedor .

  DATA: l_tabname        TYPE dd03v-tabname,
        l_fieldname      TYPE dd03v-fieldname,
        l_searchelp      TYPE shlpname,
        l_dynnr          TYPE sy-dynnr  VALUE '0002',
        l_dynprofld      TYPE dynfnam   VALUE 'ZDCO_PRODUTOR-ID_FORNECEDOR',
        vg_lifnr         TYPE lifnr,
        t_dynpfields     TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1
                              WITH HEADER LINE,
        t_ret            TYPE TABLE OF ddshretval.

  DATA: st_ret TYPE ddshretval.

* Chama o matchCode
  MOVE: 'ZDCO_PRODUTOR'      TO l_tabname,
        'ID_FORNECEDOR'      TO l_fieldname,
        'KRED'               TO l_searchelp.

* Exibe Ajuda de Pesquisa
  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = l_tabname
      fieldname         = l_fieldname
      searchhelp        = l_searchelp
      dynpprog          = sy-repid
      dynpnr            = l_dynnr
      dynprofield       = l_dynprofld
    TABLES
      return_tab        = t_ret
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = st_ret-fieldval
    IMPORTING
      output = vg_lifnr.

  SELECT SINGLE name1 INTO vg_nome_fornecedor
    FROM lfa1
   WHERE lifnr EQ vg_lifnr.

  IF sy-subrc NE 0.
    CLEAR: vg_nome_fornecedor.
  ENDIF.

  MOVE: 'VG_NOME_FORNECEDOR' TO  t_dynpfields-fieldname,
        vg_nome_fornecedor   TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZDCO_PRODUTOR-ID_FORNECEDOR' TO  t_dynpfields-fieldname,
        st_ret-fieldval               TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = l_dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDFORM.                    " Z_SHOW_MATCH_FORNECEDOR
*&---------------------------------------------------------------------*
*&      Form  Z_SHOW_MATCH_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_show_match_material .

  DATA: l_tabname        TYPE dd03v-tabname,
        l_fieldname      TYPE dd03v-fieldname,
        l_searchelp      TYPE shlpname,
        l_dynnr          TYPE sy-dynnr  VALUE '0002',
        l_dynprofld      TYPE dynfnam   VALUE 'ZDCO_PRODUTOR-CD_MATERIAL',
        vg_matnr         TYPE matnr,
        t_dynpfields     TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1
                              WITH HEADER LINE,
        t_ret            TYPE TABLE OF ddshretval.

  DATA: st_ret TYPE ddshretval.

* Chama o matchCode
  MOVE: 'ZDCO_PRODUTOR'    TO l_tabname,
        'CD_MATERIAL'      TO l_fieldname,
        'MAT1'             TO l_searchelp.

* Exibe Ajuda de Pesquisa
  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = l_tabname
      fieldname         = l_fieldname
      searchhelp        = l_searchelp
      dynpprog          = sy-repid
      dynpnr            = l_dynnr
      dynprofield       = l_dynprofld
    TABLES
      return_tab        = t_ret
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input  = st_ret-fieldval
    IMPORTING
      output = vg_matnr.

  SELECT SINGLE maktx INTO vg_nome_material
    FROM makt
   WHERE matnr EQ vg_matnr
     AND spras EQ sy-langu.

  IF sy-subrc NE 0.
    CLEAR: vg_nome_material.
  ENDIF.

  MOVE: 'VG_NOME_MATERIAL'  TO  t_dynpfields-fieldname,
        vg_nome_material    TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZDCO_PRODUTOR-CD_MATERIAL' TO  t_dynpfields-fieldname,
        st_ret-fieldval             TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = l_dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDFORM.                    " Z_SHOW_MATCH_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  Z_SHOW_MATCH_TIPO_LEILAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_show_match_tipo_leilao .

  TYPES:
     BEGIN OF ty_help,
       cd_tipo_leilao TYPE zcd_tipo_leilao,
       sg_tipo_leilao TYPE zsg_tipo_leilao,
       ds_tipo_leilao TYPE zds_tipo_leilao,
     END OF ty_help.

  DATA: t_dynpfields TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1
                          WITH HEADER LINE,
        ti_help      TYPE STANDARD TABLE OF ty_help
                          INITIAL SIZE 0
                          WITH HEADER LINE,
        t_ret        TYPE TABLE OF ddshretval.

  DATA: st_ret TYPE ddshretval.

  SELECT cd_tipo_leilao sg_tipo_leilao ds_tipo_leilao
    INTO TABLE ti_help
    FROM zdco_tipo_leilao
   ORDER BY cd_tipo_leilao.

  CHECK NOT ti_help[] IS INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'CD_TIPO_LEILAO'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_help[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  READ TABLE ti_help WITH KEY cd_tipo_leilao = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZDCO_PRODUTOR-CD_TIPO_LEILAO' TO  t_dynpfields-fieldname,
        st_ret-fieldval                TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'VG_TIPO_LEILAO'       TO  t_dynpfields-fieldname,
        ti_help-ds_tipo_leilao TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.


ENDFORM.                    " Z_SHOW_MATCH_TIPO_LEILAO


*&---------------------------------------------------------------------*
*&      Form  ZINICIALIZAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zinicializar.

  CLEAR: vg_nome_fornecedor,
         vg_nome_centro,
         vg_nome_material,
         vg_tipo_leilao,
         vg_nome_deposito,
         vg_saldo_entregue,
         vg_saldo_remessa.

ENDFORM.                    "ZINICIALIZAR


*&---------------------------------------------------------------------*
*&      Form  ZCARREGAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zcarregar .

  DATA: vg_matnr TYPE matnr,
        vg_lifnr TYPE lifnr.

  SELECT SINGLE ds_tipo_leilao
    INTO vg_tipo_leilao
    FROM zdco_tipo_leilao
   WHERE cd_tipo_leilao EQ zdco_produtor-cd_tipo_leilao.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = zdco_produtor-id_fornecedor
    IMPORTING
      output = vg_lifnr.

  SELECT SINGLE name1 INTO vg_nome_fornecedor
    FROM lfa1
   WHERE lifnr EQ vg_lifnr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input  = zdco_produtor-cd_material
    IMPORTING
      output = vg_matnr.

  SELECT SINGLE maktx
    INTO vg_nome_material
    FROM makt
   WHERE matnr EQ vg_matnr
     AND spras EQ sy-langu.

  SELECT SINGLE name1 INTO vg_nome_centro
    FROM t001w
   WHERE werks EQ zdco_produtor-cd_centro.

  SELECT SINGLE lgobe INTO vg_nome_deposito
    FROM t001l
   WHERE werks EQ zdco_produtor-cd_centro
     AND lgort EQ zdco_produtor-cd_safra.

  IF ( not zdco_produtor-qt_entregue IS INITIAL ) AND ( zdco_produtor-qt_entregue GT 0 ).
    vg_saldo_entregue = zdco_produtor-qt_material - zdco_produtor-qt_entregue.
  ELSE.
    vg_saldo_entregue = zdco_produtor-qt_material.
  ENDIF.

  IF ( not zdco_produtor-qt_remessa IS INITIAL ) AND ( zdco_produtor-qt_remessa GT 0 ).
    vg_saldo_remessa = zdco_produtor-qt_material - zdco_produtor-qt_remessa.
  ELSE.
    vg_saldo_remessa = zdco_produtor-qt_material.
  ENDIF.

ENDFORM.                    " ZCARREGAR


*&---------------------------------------------------------------------*
*&      Form  Z_SHOW_MATCH_CENTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_show_match_centro .

  DATA: l_tabname        TYPE dd03v-tabname,
        l_fieldname      TYPE dd03v-fieldname,
        l_searchelp      TYPE shlpname,
        l_dynnr          TYPE sy-dynnr  VALUE '0002',
        l_dynprofld      TYPE dynfnam   VALUE 'ZDCO_PRODUTOR-CD_CENTRO',
        t_dynpfields     TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1
                              WITH HEADER LINE,
        t_ret            TYPE TABLE OF ddshretval.

  DATA: st_ret TYPE ddshretval.

* Chama o matchCode
  MOVE: 'ZDCO_PRODUTOR'    TO l_tabname,
        'CD_CENTRO'        TO l_fieldname,
        'H_T001W_C'        TO l_searchelp.

* Exibe Ajuda de Pesquisa
  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = l_tabname
      fieldname         = l_fieldname
      searchhelp        = l_searchelp
      dynpprog          = sy-repid
      dynpnr            = l_dynnr
      dynprofield       = l_dynprofld
    TABLES
      return_tab        = t_ret
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE name1 INTO vg_nome_centro
    FROM t001w
   WHERE werks EQ st_ret-fieldval.

  IF sy-subrc NE 0.
    CLEAR: vg_nome_centro.
  ENDIF.

  MOVE: 'VG_NOME_CENTRO'  TO  t_dynpfields-fieldname,
        vg_nome_centro      TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZDCO_PRODUTOR-CD_CENTRO' TO  t_dynpfields-fieldname,
        st_ret-fieldval           TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.


* Imprime no campo da tela
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = l_dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDFORM.                    " Z_SHOW_MATCH_CENTRO


*&---------------------------------------------------------------------*
*&      Form  Z_SHOW_MATCH_SAFRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_show_match_safra .

  TYPES:
     BEGIN OF ty_help,
       lgort TYPE lgort_d,
       lgobe TYPE lgobe,
     END OF ty_help.

  DATA: vg_centro        TYPE rkb1t-character,
        t_dynpfields TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1
                          WITH HEADER LINE,
        ti_help      TYPE STANDARD TABLE OF ty_help
                          INITIAL SIZE 0
                          WITH HEADER LINE,
        t_ret        TYPE TABLE OF ddshretval.

  DATA: st_ret TYPE ddshretval.

  CALL FUNCTION 'DYNPRO_STRING_READ'
    EXPORTING
      i_fieldname = 'ZDCO_PRODUTOR-CD_CENTRO'
      i_repid     = sy-repid
    IMPORTING
      e_string    = vg_centro.

  IF vg_centro IS INITIAL.

    MESSAGE 'Favor informar o centro para consulta.' TYPE 'I'.

  ELSE.

    SELECT lgort lgobe
      INTO TABLE ti_help
      FROM t001l
     WHERE werks EQ vg_centro
     ORDER BY lgort.

    CHECK NOT ti_help[] IS INITIAL.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield   = 'LGORT'
        dynpprog   = sy-repid
        dynpnr     = sy-dynnr
        value_org  = 'S'
      TABLES
        value_tab  = ti_help[]
        return_tab = t_ret.

    READ TABLE t_ret INTO st_ret INDEX 1.
    CHECK sy-subrc IS INITIAL.

    READ TABLE ti_help WITH
    KEY lgort = st_ret-fieldval BINARY SEARCH.

    MOVE: 'ZDCO_PRODUTOR-CD_SAFRA' TO  t_dynpfields-fieldname,
          st_ret-fieldval          TO  t_dynpfields-fieldvalue.
    APPEND t_dynpfields.

    MOVE: 'VG_NOME_DEPOSITO'  TO  t_dynpfields-fieldname,
          ti_help-lgobe       TO  t_dynpfields-fieldvalue.
    APPEND t_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = t_dynpfields.

  ENDIF.


ENDFORM.                    " Z_SHOW_MATCH_SAFRA


*&---------------------------------------------------------------------*
*&      Form  ZNU_DCO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM znu_dco.

  CHECK zdco_produtor-nu_dco IS INITIAL.

  DATA: vn_dco TYPE znu_dco,
        t_dynpfields TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1
                          WITH HEADER LINE.

  SELECT MAX( nu_dco )
    INTO vn_dco
    FROM zdco_produtor.

  IF vn_dco IS INITIAL.
    vn_dco = 1.
  ELSE.
    vn_dco = vn_dco + 1.
  ENDIF.

  zdco_produtor-nu_dco = vn_dco.

ENDFORM.                    "ZNU_DCO

*&---------------------------------------------------------------------*
*&      Form  ZVERIFICA_ENTRADAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zverifica_entradas .

  IF zdco_produtor-qt_material LT zdco_produtor-qt_entregue.
    MESSAGE 'A Quantidade do DCO não pode ser menor que a quantidade entregue!' TYPE 'E'.
  ENDIF.

  IF zdco_produtor-qt_material LT zdco_produtor-qt_remessa.
    MESSAGE 'A Quantidade do DCO não pode ser menor que a quantidade da remessa!' TYPE 'E'.
  ENDIF.

ENDFORM.                    " ZVERIFICA_ENTRADAS
*&---------------------------------------------------------------------*
*&      Form  Z_SHOW_MATCH_DOC_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_show_match_doc_venda .

  TYPES:
     BEGIN OF ty_help,
       vkorg LIKE vbak-vkorg,  "Organização de vendas
       vtweg LIKE vbak-vtweg,  "Canal de distribuição
       spart LIKE vbak-spart,  "Setor de atividade
       vkbur LIKE vbak-vkbur,  "Escritório de vendas
       vkgrp LIKE vbak-vkgrp,  "Equipe de vendas
       kunnr LIKE vbak-kunnr,  "Emissor da ordem
       ernam LIKE vbak-ernam,  "Nome do responsável que adicionou o objeto
       erdat LIKE vbak-erdat,  "Data de criação do registro
       vbeln LIKE vbak-vbeln,  "Documento de vendas
     END OF ty_help.

  DATA: t_dynpfields TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1
                          WITH HEADER LINE,
        ti_help      TYPE STANDARD TABLE OF ty_help
                          INITIAL SIZE 0
                          WITH HEADER LINE,
        t_ret        TYPE TABLE OF ddshretval.

  DATA: st_ret TYPE ddshretval.

  IF zdco_produtor-cd_centro IS INITIAL.
    MESSAGE 'Favor informar o centro para consulta.' TYPE 'I'.
  ELSE.

    SELECT vkorg vtweg spart vkbur vkgrp
           kunnr ernam erdat vbeln
      INTO TABLE ti_help
      FROM vbak AS cb
     WHERE auart EQ 'ZRDC'
       AND ( vbeln IN ( SELECT vbeln FROM vbap AS it WHERE it~vbeln EQ cb~vbeln AND it~gsber EQ zdco_produtor-cd_centro )
        OR NOT EXISTS ( SELECT * FROM vbap AS itt WHERE itt~vbeln EQ cb~vbeln ) )
     ORDER BY vbeln.

    IF NOT ti_help[] IS INITIAL.

      CHECK NOT ti_help[] IS INITIAL.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield   = 'vbeln'
          dynpprog   = sy-repid
          dynpnr     = sy-dynnr
          value_org  = 'S'
        TABLES
          value_tab  = ti_help[]
          return_tab = t_ret.

      READ TABLE t_ret INTO st_ret INDEX 1.
      CHECK sy-subrc IS INITIAL.

      READ TABLE ti_help WITH KEY vbeln = st_ret-fieldval BINARY SEARCH.

      MOVE: 'ZDCO_PRODUTOR-VBELN' TO  t_dynpfields-fieldname,
            st_ret-fieldval       TO  t_dynpfields-fieldvalue.
      APPEND t_dynpfields.

      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname     = sy-repid
          dynumb     = sy-dynnr
        TABLES
          dynpfields = t_dynpfields.

    ELSE.
      DATA menssagem TYPE string.
      CONCATENATE 'Não existe documento de vendas do tipo ZRDC para o centro' zdco_produtor-cd_centro '!' INTO menssagem
      SEPARATED BY space.
      MESSAGE menssagem TYPE 'I'.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_SHOW_MATCH_DOC_VENDA
*&---------------------------------------------------------------------*
*&      Form  ZTRAVACAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ztravacampos .

  LOOP AT SCREEN.
    IF ( screen-name = 'ZDCO_PRODUTOR-CD_MATERIAL'    ) OR ( screen-name = 'ZDCO_PRODUTOR-ID_FORNECEDOR' ) OR
       ( screen-name = 'ZDCO_PRODUTOR-CD_CENTRO'      ) OR ( screen-name = 'ZDCO_PRODUTOR-CD_SAFRA' )      OR
       ( screen-name = 'ZDCO_PRODUTOR-CD_TIPO_LEILAO' ) OR
       ( ( screen-name = 'ZDCO_PRODUTOR-VBELN' )    AND ( zdco_produtor-qt_remessa GT 0 ) )       OR
       ( ( screen-name = 'ZDCO_PRODUTOR-NU_AVISO' ) AND ( zdco_produtor-qt_remessa GT 0 ) ).
      IF ( zdco_produtor-qt_entregue GT 0 ) OR ( zdco_produtor-qt_remessa GT 0 ) .
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ZTRAVACAMPOS

*&---------------------------------------------------------------------*
*&      Form  ZCALCULA_SALDO_ENTREGUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zcalcula_saldo_entregue .

  BREAK-POINT.

ENDFORM.                    " ZCALCULA_SALDO_ENTREGUE
