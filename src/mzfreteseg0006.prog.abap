*----------------------------------------------------------------------*
***INCLUDE MZFRETESEG0006 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0006  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0006 OUTPUT.

  IF NOT vg_insert IS INITIAL.
    SET PF-STATUS 'PFINSERT'.
    SET TITLEBAR 'TLINSERT'.
    vg_pesquis = 'X'.
  ELSE.
    IF vg_editar IS INITIAL.
      SET PF-STATUS 'PFCONSUL'.
      SET TITLEBAR 'TLCONSUL'.
      CLEAR: vg_pesquis.
    ELSE.
      SET PF-STATUS 'PFEDIT'.
      SET TITLEBAR 'TLEDIT'.
      vg_pesquis = 'X'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " STATUS_0006  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0006  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0006 INPUT.

  CASE ok_code.
    WHEN 'INSERIR'.
      PERFORM inserir_novo.
    WHEN 'EDITAR'.
      vg_editar = 'X'.
    WHEN 'SALVAR'.
      PERFORM salvar.
      CLEAR: vg_editar.
    WHEN 'QUIT' OR 'NOVOC'.
      IF ( vg_editar IS NOT INITIAL ).
        CLEAR: vg_editar.
      ELSE.
        IF ok_code EQ 'QUIT'.
          CLEAR ok_code.
        ENDIF.
        PERFORM sair.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0006  INPUT

*&---------------------------------------------------------------------*
*&      Form  INSERIR_NOVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM inserir_novo .

  DATA: answer TYPE c LENGTH 1.

  SELECT SINGLE * INTO wa_seg_terc
    FROM zvalor_seg_terc
   WHERE monat          EQ zvalor_seg_terc-monat
     AND gjahr          EQ zvalor_seg_terc-gjahr
     AND cd_pais_ini    EQ zvalor_seg_terc-cd_pais_ini
     AND cd_cidade_ini  EQ zvalor_seg_terc-cd_cidade_ini
     AND cd_uf_ini      EQ zvalor_seg_terc-cd_uf_ini
     AND cd_pais_fim    EQ zvalor_seg_terc-cd_pais_fim
     AND cd_cidade_fim  EQ zvalor_seg_terc-cd_cidade_fim
     AND cd_uf_fim      EQ zvalor_seg_terc-cd_uf_fim
     AND cd_material    EQ zvalor_seg_terc-cd_material
     AND cd_moeda       EQ zvalor_seg_terc-cd_moeda
     AND dt_inicio      EQ zvalor_seg_terc-dt_inicio.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        titel     = 'Atenção!'
        textline1 = 'Registro já cadastrado.'
        textline2 = 'Deseja subscrever?'
      IMPORTING
        answer    = answer.

    CASE answer.
      WHEN 'J'.
        PERFORM salvar.
      WHEN 'N'.
        LEAVE TO SCREEN 0.
      WHEN 'A'.
        EXIT.
    ENDCASE.
  ELSE.
    PERFORM salvar.
  ENDIF.

ENDFORM.                    " INSERIR_NOVO

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_update_flag INPUT.
  vg_alterou = 'X'.
ENDMODULE.                 " SET_UPDATE_FLAG  INPUT

*&---------------------------------------------------------------------*
*&      Form  SALVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM salvar .

  DATA: vg_dt_inicio_atual TYPE datum.

  IF vg_alterou IS NOT INITIAL.

    IF zvalor_seg_terc-cd_pais_ini IS INITIAL.
      MESSAGE 'Deve ser informado o pais de início da prestação de serviço!' TYPE 'S'.
      EXIT.
    ENDIF.
    IF zvalor_seg_terc-cd_cidade_ini IS INITIAL.
      MESSAGE 'Deve ser informado a cidade de início da prestação de serviço!' TYPE 'S'.
      EXIT.
    ENDIF.
    IF zvalor_seg_terc-cd_uf_ini IS INITIAL.
      MESSAGE 'Deve ser informado a UF de início da prestação de serviço!' TYPE 'S'.
      EXIT.
    ENDIF.
    IF zvalor_seg_terc-cd_pais_fim IS INITIAL.
      MESSAGE 'Deve ser informado o pais de fim da prestação de serviço!' TYPE 'S'.
      EXIT.
    ENDIF.
    IF zvalor_seg_terc-cd_cidade_fim IS INITIAL.
      MESSAGE 'Deve ser informado a cidade de fim da prestação de serviço!' TYPE 'S'.
      EXIT.
    ENDIF.
    IF zvalor_seg_terc-cd_uf_fim IS INITIAL.
      MESSAGE 'Deve ser informado a UF de fim da prestação de serviço!' TYPE 'S'.
      EXIT.
    ENDIF.
    IF zvalor_seg_terc-cd_material IS INITIAL.
      MESSAGE 'Deve ser informado o material!' TYPE 'S'.
      EXIT.
    ENDIF.
    IF zvalor_seg_terc-dt_inicio IS INITIAL.
      MESSAGE 'Deve ser informado a data de inicio de valor de seguro de frete!' TYPE 'S'.
      EXIT.
    ENDIF.
    IF zvalor_seg_terc-dt_inicio LT sy-datum.
      MESSAGE 'Data de inicio menor que data atual!' TYPE 'S'.
      EXIT.
    ENDIF.
    IF zvalor_seg_terc-vr_fre_ton IS INITIAL.
      MESSAGE 'Deve ser informado o valor do frete!' TYPE 'S'.
      EXIT.
    ENDIF.
    IF zvalor_seg_terc-vr_mer_ton IS INITIAL.
      MESSAGE 'Deve ser informado o valor da mercadores!' TYPE 'S'.
      EXIT.
    ENDIF.

    CLEAR: vg_alterou, vg_insert, vg_editar.
    SELECT SINGLE * INTO wa_valor_taxa
      FROM zvalor_seg_taxa
     WHERE cd_moeda EQ zvalor_seg_terc-cd_moeda.

    zvalor_seg_terc-vr_imp_ton = ( zvalor_seg_terc-vr_mer_ton / wa_valor_taxa-vr_taxa ) - zvalor_seg_terc-vr_mer_ton.
    zvalor_seg_terc-vr_tot_ton = zvalor_seg_terc-vr_fre_ton + zvalor_seg_terc-vr_mer_ton + zvalor_seg_terc-vr_imp_ton.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_seg_terc
      FROM zvalor_seg_terc
     WHERE cd_pais_ini    EQ zvalor_seg_terc-cd_pais_ini
       AND cd_cidade_ini  EQ zvalor_seg_terc-cd_cidade_ini
       AND cd_uf_ini      EQ zvalor_seg_terc-cd_uf_ini
       AND cd_pais_fim    EQ zvalor_seg_terc-cd_pais_fim
       AND cd_cidade_fim  EQ zvalor_seg_terc-cd_cidade_fim
       AND cd_uf_fim      EQ zvalor_seg_terc-cd_uf_fim
       AND cd_material    EQ zvalor_seg_terc-cd_material
       AND cd_moeda       EQ zvalor_seg_terc-cd_moeda.

    vg_dt_inicio_atual = zvalor_seg_terc-dt_inicio.

    LOOP AT it_seg_terc INTO wa_seg_terc.
      IF wa_seg_terc-dt_final IS INITIAL.
        wa_seg_terc-dt_final = vg_dt_inicio_atual.
        MODIFY zvalor_seg_terc FROM wa_seg_terc.
      ENDIF.
    ENDLOOP.

    MODIFY zvalor_seg_terc.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " SALVAR

*&---------------------------------------------------------------------*
*&      Module  F_DINICIO_TAXJURCODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_dinicio_taxjurcode INPUT.

  DATA: ti_help  TYPE STANDARD TABLE OF ty_help_domicilio INITIAL SIZE 0 WITH HEADER LINE.
  DATA: l_country       TYPE land1_gp,
        l_estado        TYPE regio.

  DATA: BEGIN OF dynpfields OCCURS 2.
          INCLUDE STRUCTURE dynpread.
  DATA: END   OF dynpfields.

  CHECK NOT vg_pesquis IS INITIAL.

  dynpfields-fieldname = 'ZVALOR_SEG_TERC-CD_PAIS_INI'.
  APPEND dynpfields.

  dynpfields-fieldname = 'ZVALOR_SEG_TERC-CD_UF_INI'.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = 'SAPMZFRETESEG'
      dynumb               = '0006'
    TABLES
      dynpfields           = dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      OTHERS               = 10.

  READ TABLE dynpfields WITH KEY fieldname = 'ZVALOR_SEG_TERC-CD_PAIS_INI'.
  l_country = dynpfields-fieldvalue.

  READ TABLE dynpfields WITH KEY fieldname = 'ZVALOR_SEG_TERC-CD_UF_INI'.
  l_estado = dynpfields-fieldvalue.

  IF l_country IS INITIAL.
    MESSAGE 'Favor informar o país de início da prestação!' TYPE 'S'.
    STOP.
  ENDIF.

  IF l_estado IS INITIAL.

    SELECT cep~country
           cep~region
           cep~taxjurcode
           cid~text
      INTO CORRESPONDING FIELDS OF TABLE ti_help
      FROM j_1btreg_city AS cep
      INNER JOIN j_1btxjurt AS cid ON cid~country EQ cep~country
                               AND cid~taxjurcode EQ cep~taxjurcode
     WHERE cep~country EQ l_country.

  ELSE.

    SELECT cep~country
           cep~region
           cep~taxjurcode
           cid~text
      INTO CORRESPONDING FIELDS OF TABLE ti_help
      FROM j_1btreg_city AS cep
      INNER JOIN j_1btxjurt AS cid ON cid~country EQ cep~country
                               AND cid~taxjurcode EQ cep~taxjurcode
     WHERE cep~country EQ l_country
       AND cep~region  EQ l_estado.

  ENDIF.

  SORT ti_help[] BY taxjurcode.

  CHECK NOT ti_help[] IS INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_help[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  READ TABLE ti_help WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZVALOR_SEG_TERC-CD_PAIS_INI'   TO  t_dynpfields-fieldname,
        ti_help-country                 TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZVALOR_SEG_TERC-CD_UF_INI'     TO  t_dynpfields-fieldname,
        ti_help-region                  TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'VG_NOME_CIDADE_INI'            TO  t_dynpfields-fieldname,
        ti_help-text                    TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZVALOR_SEG_TERC-CD_CIDADE_INI' TO  t_dynpfields-fieldname,
        ti_help-taxjurcode              TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " F_DINICIO_TAXJURCODE  INPUT

*&---------------------------------------------------------------------*
*&      Module  RECUPERA_INFO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE recupera_info OUTPUT.

  CLEAR: vg_nome_cidade_ini, vg_nome_cidade_fim, vg_nome_produto.

  IF ( zvalor_seg_terc-cd_pais_ini IS NOT INITIAL ) AND
     ( zvalor_seg_terc-cd_cidade_ini IS NOT INITIAL ) AND
     ( zvalor_seg_terc-cd_uf_ini IS NOT INITIAL ).

    SELECT SINGLE text
      INTO vg_nome_cidade_ini
      FROM j_1btxjurt
     WHERE taxjurcode EQ zvalor_seg_terc-cd_cidade_ini
       AND country    EQ zvalor_seg_terc-cd_pais_ini
       AND spras      EQ sy-langu.

  ENDIF.

  IF ( zvalor_seg_terc-cd_pais_fim IS NOT INITIAL ) AND
     ( zvalor_seg_terc-cd_cidade_fim IS NOT INITIAL ) AND
     ( zvalor_seg_terc-cd_uf_fim IS NOT INITIAL ).

    SELECT SINGLE text
      INTO vg_nome_cidade_fim
      FROM j_1btxjurt
     WHERE taxjurcode EQ zvalor_seg_terc-cd_cidade_fim
       AND country    EQ zvalor_seg_terc-cd_pais_fim
       AND spras      EQ sy-langu.

  ENDIF.

  IF NOT zvalor_seg_terc-cd_material IS INITIAL.
    SELECT SINGLE maktx INTO vg_nome_produto
      FROM makt
     WHERE matnr EQ zvalor_seg_terc-cd_material
       AND spras EQ sy-langu.
  ENDIF.

ENDMODULE.                 " RECUPERA_INFO  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  F_DFINAL_TAXJURCODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_dfinal_taxjurcode INPUT.

  DATA: ti_help_fim     TYPE STANDARD TABLE OF ty_help_domicilio INITIAL SIZE 0 WITH HEADER LINE.
  DATA: l_country_fim   TYPE land1_gp,
        l_estado_fim    TYPE regio.

  DATA: BEGIN OF dynpfieldsf OCCURS 2.
          INCLUDE STRUCTURE dynpread.
  DATA: END   OF dynpfieldsf.

  CHECK NOT vg_pesquis IS INITIAL.

  dynpfieldsf-fieldname = 'ZVALOR_SEG_TERC-CD_PAIS_FIM'.
  APPEND dynpfieldsf.

  dynpfieldsf-fieldname = 'ZVALOR_SEG_TERC-CD_UF_FIM'.
  APPEND dynpfieldsf.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = 'SAPMZFRETESEG'
      dynumb               = '0006'
    TABLES
      dynpfields           = dynpfieldsf
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      OTHERS               = 10.

  READ TABLE dynpfieldsf WITH KEY fieldname = 'ZVALOR_SEG_TERC-CD_PAIS_FIM'.
  l_country_fim = dynpfieldsf-fieldvalue.

  READ TABLE dynpfieldsf WITH KEY fieldname = 'ZVALOR_SEG_TERC-CD_UF_FIM'.
  l_estado_fim = dynpfieldsf-fieldvalue.

  IF l_country_fim IS INITIAL.
    MESSAGE 'Favor informar o país de fim da prestação!' TYPE 'S'.
    STOP.
  ENDIF.

  IF l_estado_fim IS INITIAL.

    SELECT cep~country
           cep~region
           cep~taxjurcode
           cid~text
      INTO CORRESPONDING FIELDS OF TABLE ti_help_fim
      FROM j_1btreg_city AS cep
      INNER JOIN j_1btxjurt AS cid ON cid~country EQ cep~country
                               AND cid~taxjurcode EQ cep~taxjurcode
     WHERE cep~country EQ l_country_fim.

  ELSE.

    SELECT cep~country
           cep~region
           cep~taxjurcode
           cid~text
      INTO CORRESPONDING FIELDS OF TABLE ti_help_fim
      FROM j_1btreg_city AS cep
      INNER JOIN j_1btxjurt AS cid ON cid~country EQ cep~country
                               AND cid~taxjurcode EQ cep~taxjurcode
     WHERE cep~country EQ l_country_fim
       AND cep~region  EQ l_estado_fim.

  ENDIF.

  SORT ti_help_fim[] BY taxjurcode.

  CHECK NOT ti_help_fim[] IS INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_help_fim[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  READ TABLE ti_help_fim WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZVALOR_SEG_TERC-CD_PAIS_FIM'   TO  t_dynpfields-fieldname,
        ti_help_fim-country             TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZVALOR_SEG_TERC-CD_UF_FIM'     TO  t_dynpfields-fieldname,
        ti_help_fim-region              TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'VG_NOME_CIDADE_FIM'            TO  t_dynpfields-fieldname,
        ti_help_fim-text                TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZVALOR_SEG_TERC-CD_CIDADE_FIM' TO  t_dynpfields-fieldname,
        ti_help_fim-taxjurcode          TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " F_DFINAL_TAXJURCODE  INPUT

*&---------------------------------------------------------------------*
*&      Form  SAIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sair .

  DATA: answer TYPE c LENGTH 1.

  IF NOT vg_alterou IS INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        titel     = 'Atenção!'
        textline1 = 'Dados foram alterados.'
        textline2 = 'Deseja salvar?'
      IMPORTING
        answer    = answer.

    CASE answer.
      WHEN 'J'.
        PERFORM salvar.
        LEAVE TO SCREEN 0.
      WHEN 'N'.
        CLEAR: vg_alterou, vg_pesquis.
        LEAVE TO SCREEN 0.
      WHEN 'A'.
        EXIT.
    ENDCASE.

  ELSE.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " SAIR

*&---------------------------------------------------------------------*
*&      Module  CAMPOS_VISIVEIS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE campos_visiveis OUTPUT.

*  LOOP AT SCREEN.
*    IF  ( screen-name(15) EQ 'ZVALOR_SEG_TERC' ).
*      IF ( NOT vg_insert IS INITIAL ).
*        screen-output = '0'.
*        screen-input  = '1'.
*        MODIFY SCREEN.
*      ELSEIF ( vg_editar IS NOT INITIAL ).
*        screen-output = '0'.
*        screen-input  = '1'.
*        MODIFY SCREEN.
*      ELSE.
*        screen-output = '1'.
*        screen-input  = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*
*      IF screen-name EQ 'ZVALOR_SEG_TERC-VR_IMP_TON' OR
*         screen-name EQ 'ZVALOR_SEG_TERC-VR_TOT_TON' OR
*         screen-name EQ 'ZVALOR_SEG_TERC-DT_FINAL'.
*        screen-output = '1'.
*        screen-input  = '0'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

ENDMODULE.                 " CAMPOS_VISIVEIS  OUTPUT
