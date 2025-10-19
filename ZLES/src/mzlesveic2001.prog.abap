DATA: v_nome_fornec TYPE lfa1-name1,
      v_bahns       TYPE lfa1-bahns,
      v_stkzn       TYPE lfa1-stkzn,
      v_stcd1       TYPE lfa1-stcd1,
      v_stcd3       TYPE lfa1-stcd3,
      g_dialog_mode LIKE szad_field-maint_mode,
      display       LIKE szad_field-maint_mode  VALUE 'DISPLAY'.


*----------------------------------------------------------------------*
***INCLUDE MZLESVEIC2001 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_2001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*-------------------------------------------------------------------- --*
MODULE status_2001 OUTPUT.

*  CLEAR: NOME_FORNEC,
*         CNPJ_FORN,
*         CPF_FORN,
*         BAHNS.

  DATA: lo_container TYPE REF TO cl_gui_custom_container.

  IF vg_2001 EQ 'NOVO'.
    SET PF-STATUS 'PFNOVO'.
    SET TITLEBAR 'TINOVO'.
  ELSEIF vg_2001 EQ 'CONS'.
    SET PF-STATUS 'PFCONS'.
    SET TITLEBAR 'TICONS'.
  ELSEIF vg_2001 EQ 'EDIT'.
    SET PF-STATUS 'PFEDIT'.
    SET TITLEBAR 'TIEDIT'.
  ENDIF.

  PERFORM trava_campos.

  ls_object-objtype = 'ZGOS'.

  CREATE OBJECT go_myobject
    EXPORTING
      is_object    = ls_object
      ip_no_commit = 'R'.


ENDMODULE.                 " STATUS_2001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2001 INPUT.

  IF sy-ucomm IS NOT INITIAL.

    CASE vg_modal.
        "CONSULTA
      WHEN 'ALTERAR'.
        vg_2001 = 'EDIT'.
        vg_pesquis = 'X'.
      WHEN 'SAIR'.
        PERFORM sair.
        IF vg_alterou IS INITIAL.
          LEAVE TO SCREEN 0.
        ENDIF.
        "EDIÇÃO
      WHEN 'SAVEDIT'.
        PERFORM salvar.
      WHEN 'CANCEDIT'.
        vg_2001 = 'CONS'.
        PERFORM cancelar.
      WHEN 'SAVNOVO'.
        PERFORM salvar.
      WHEN 'SAIRNOVO'.
        PERFORM sair.
        IF vg_alterou IS INITIAL.
          LEAVE TO SCREEN 0.
        ENDIF.
      WHEN 'TESTE'  .
        "perform teste.
    ENDCASE.

  ENDIF.
ENDMODULE.                 " USER_COMMAND_2001  INPUT

*&---------------------------------------------------------------------*
*&      Form  TRAVA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM trava_campos .

  LOOP AT SCREEN.
    CHECK screen-name NE 'ZLEST0002-KALSM'.
    CHECK screen-name NE 'ZLEST0002-SPRAS'.


    IF ( vg_2001 EQ 'CONS' ) AND ( screen-name(9) EQ 'ZLEST0002' ).
      screen-output = '1'.
      screen-input  = '0'.
      MODIFY SCREEN.
    ELSEIF ( vg_2001 EQ 'EDIT' ) AND ( screen-name(9) EQ 'ZLEST0002' ).
      IF "( SCREEN-NAME NE 'ZLEST0002-PC_VEICULO' ) AND
         ( screen-name NE 'ZLEST0002-ERDAT' ) AND
         ( screen-name NE 'ZLEST0002-ERZET' ) AND
         ( screen-name NE 'ZLEST0002-ERNAM' ) AND
         ( screen-name NE 'ZLEST0002-CD_CIDADE') AND
         ( screen-name NE 'ZLEST0002-USR_MODIFICACAO') AND
         ( screen-name NE 'ZLEST0002-DT_MODIFICACAO') AND
         ( screen-name NE 'ZLEST0002-WERKS') AND
         ( screen-name NE 'ZLEST0002-EQUNR') AND
         ( screen-name NE 'ZLEST0002-PROPRIET_COMODATO') AND
         ( screen-name NE 'ZLEST0002-NR_TAG_STRADA').    "*-CS2024001181-16.12.2024-#160856-JT

        screen-input  = '1'.
      ELSE.
        screen-output = '1'.
        screen-input  = '0'.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF ( vg_2001 EQ 'NOVO' ) AND ( screen-name EQ 'ZLEST0002-CD_CIDADE').
      screen-output = '1'.
      screen-input  = '0'.
      MODIFY SCREEN.
    ELSEIF ( vg_2001 EQ 'NOVO' ) AND ( screen-name EQ 'ZLEST0002-PROPRIETARIO' ).

      v_nome_fornec = ''.
      v_stcd1 = ''.
      v_stcd3 = ''.
      v_bahns = ''.

      SELECT SINGLE name1 bahns stcd1 stcd3
        INTO (v_nome_fornec,v_bahns,v_stcd1,v_stcd3)
        FROM lfa1
      WHERE lifnr = zlest0002-proprietario.

      nome_fornec = v_nome_fornec.
      cnpj_forn   = v_stcd1.
      cpf_forn    = v_stcd3.
      bahns       = v_bahns.

    ENDIF.

    IF ( ( vg_2001 EQ 'NOVO'  ) OR ( vg_2001 EQ 'EDIT'  ) ) AND ( screen-name EQ 'ZLEST0002-DT_VENC_CTO' ).
      IF zlest0002-cto_comodato = '1' AND screen-invisible = 1.
        screen-invisible = '0'.
        screen-input     = '1'.
      ELSE.
        screen-invisible = '1'.
        screen-input     = '0'.
        screen-output    = '1'.
      ENDIF.



      MODIFY SCREEN.
    ELSEIF ( vg_2001 EQ 'CONS'  ) AND ( screen-name EQ 'ZLEST0002-DT_VENC_CTO' ).
      screen-output = '1'.
      screen-input  = '0'.
      IF zlest0002-cto_comodato = '1' AND screen-invisible = 1.
        screen-invisible = '0'.
      ELSE.
        screen-invisible = '1'.
      ENDIF.




      MODIFY SCREEN.

    ENDIF.

*    IF ( SCREEN-NAME EQ 'ZLEST0002-TP_VEICULO' ) AND ( VG_2001 EQ 'NOVO' ).
*      SCREEN-INVISIBLE = '0'.
*      SCREEN-INPUT     = '0'.
*      SCREEN-OUTPUT    = '1'.
*      MODIFY SCREEN.
*    ENDIF.

    IF cnpj_forn(8) NE '77294254'.
      IF  screen-name EQ 'ZLEST0002-FROTA' OR screen-name EQ 'ZLEST0002-GRUPO' OR screen-name EQ 'ZLEST0002-COD_ANTENA'.
        screen-output = '1'.
        screen-input  = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

*alterado por guilherme rabelo inicio 15.06.2023
    IF  zlest0002-propriet_comodato <> ' ' .

      SELECT SINGLE name1 bahns stcd1 stcd3
        INTO (v_nome_fornec,v_bahns,v_stcd1,v_stcd3)
        FROM lfa1
      WHERE lifnr = zlest0002-propriet_comodato.

      nome_fornec2 = v_nome_fornec.
      cnpj_forn2   = v_stcd1.
      cpf_forn2    = v_stcd3.
      bahns2       = v_bahns.

    ELSE.
      zlest0002-propriet_comodato = ' '.
    ENDIF.

    IF sy-ucomm = 'ALTERAR' OR sy-ucomm = 'CT2'.

      IF  screen-name EQ 'ZLEST0002-PROPRIET_COMODATO'.

        IF zlest0002-cto_comodato = '2'.

          screen-input = '0'.
          MODIFY SCREEN.
          zlest0002-propriet_comodato = ' '.
          nome_fornec2 = ' '.
          bahns2  = ' '.
          cnpj_forn2  = ' '.
          cpf_forn2  = ' '.

        ELSEIF   zlest0002-cto_comodato = '1'.

          screen-input = '1'.
          MODIFY SCREEN.

          IF  zlest0002-propriet_comodato <> ' ' .

            SELECT SINGLE name1 bahns stcd1 stcd3
              INTO (v_nome_fornec,v_bahns,v_stcd1,v_stcd3)
              FROM lfa1
            WHERE lifnr = zlest0002-propriet_comodato.

            nome_fornec2 = v_nome_fornec.
            cnpj_forn2   = v_stcd1.
            cpf_forn2    = v_stcd3.
            bahns2       = v_bahns.

          ELSE.
            zlest0002-propriet_comodato = ' '.
          ENDIF.

        ENDIF.

      ENDIF.
    ENDIF.

*alterado por guilherme fim inicio 15.06.2023
  ENDLOOP.

ENDFORM.                    " TRAVA_CAMPOS
*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_nome_fornecedor2 INPUT.

  DATA: wl_lfa1_2 TYPE lfa1.

  CLEAR: nome_fornec2,
         cnpj_forn2,
         cpf_forn2.

  CLEAR: wl_lfa1_2.

  IF zlest0002-propriet_comodato IS NOT INITIAL.



    SELECT SINGLE * FROM lfa1
      INTO wl_lfa1_2
     WHERE lifnr = zlest0002-propriet_comodato.

    IF ( sy-subrc EQ 0 ).



      nome_fornec2 = wl_lfa1_2-name1.
      CASE wl_lfa1_2-stkzn.
        WHEN: 'X'.
          cpf_forn2    = wl_lfa1_2-stcd2.
        WHEN OTHERS.
          cnpj_forn2   = wl_lfa1_2-stcd1.

      ENDCASE.

    ELSE.
      CLEAR: nome_fornec2,
             cnpj_forn2,
             cpf_forn2.
    ENDIF.

  ELSE.
    CLEAR: nome_fornec2,
     cnpj_forn2,
     cpf_forn2.
  ENDIF.

ENDMODULE.                 " SET_UPDATE_FLAG  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_nome_fornecedor INPUT.

  DATA: wl_lfa1 TYPE lfa1.
  DATA: bool TYPE boolean,
        tam  TYPE i.

  DATA: placa_numero TYPE char4.

  CLEAR: nome_fornec,
         cnpj_forn,
         cpf_forn,
         bool,
         tam.

  CLEAR: wl_lfa1.

  IF zlest0002-proprietario IS NOT INITIAL.



    SELECT SINGLE * FROM lfa1
      INTO wl_lfa1
     WHERE lifnr = zlest0002-proprietario.

    IF ( sy-subrc EQ 0 ).



      nome_fornec = wl_lfa1-name1.
      CASE wl_lfa1-stkzn.
        WHEN: 'X'.
          cpf_forn    = wl_lfa1-stcd2.
        WHEN OTHERS.
          cnpj_forn   = wl_lfa1-stcd1.

      ENDCASE.

    ELSE.
      CLEAR: nome_fornec,
             cnpj_forn,
             cpf_forn.
    ENDIF.

  ELSE.
    CLEAR: nome_fornec,
     cnpj_forn,
     cpf_forn.
  ENDIF.

*  CLEAR: PLACA_NUMERO.
*  PLACA_NUMERO = ZLEST0002-PC_VEICULO(03).
*  "Validar a placa se ela é valida o não.
*  IF ( PLACA_NUMERO(03) CO SY-ABCDE ).
*    BOOL = '-'.
*  ELSE.
*    BOOL = 'X'.
*  ENDIF.
*
*  PLACA_NUMERO = ZLEST0002-PC_VEICULO+03.
*  IF ( PLACA_NUMERO(04) CO '0123456789' ).
*    BOOL = '-'.
*  ELSE.
*    BOOL = 'X'.
*  ENDIF.
*
*  CASE BOOL.
*    WHEN: 'X'.
*      MESSAGE 'Placa precisa ter o formato AAA1111' TYPE 'I'.
*
*  ENDCASE.


ENDMODULE.                 " SET_UPDATE_FLAG  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_bahns_fornecedor INPUT.

  v_bahns = ''.
  IF zlest0002-proprietario IS NOT INITIAL.
    SELECT SINGLE bahns
      INTO v_bahns
      FROM lfa1
     WHERE lifnr = zlest0002-proprietario.
  ELSE.
    v_bahns = ''.
  ENDIF.

  bahns = v_bahns.




  IF zlest0002-propriet_comodato IS NOT INITIAL.
    SELECT SINGLE bahns
      INTO v_bahns
      FROM lfa1
     WHERE lifnr = zlest0002-propriet_comodato.
  ELSE.
    v_bahns = ''.
  ENDIF.

  bahns2 = v_bahns.

ENDMODULE.                 " SET_UPDATE_FLAG  INPUT



*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_update_flag INPUT.
  vg_alterou = 'X'.



ENDMODULE.                 " SET_UPDATE_FLAG  INPUT


*&---------------------------------------------------------------------*
*&      Form  CARREGA_DADOS_ATUALIZADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carrega_dados_atualizado.
  zlest0002-erdat	= sy-datlo.
  zlest0002-erzet	= sy-timlo.
  zlest0002-ernam	= sy-uname.
ENDFORM.                    " CARREGA_DADOS_ATUALIZADO

*&---------------------------------------------------------------------*
*&      Form  SALVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM salvar .

  DATA erro TYPE c LENGTH 1.
  DATA vg_erdat      TYPE erdat.
  DATA vg_erzet      TYPE erzet.
  DATA: wl_zlest0047 TYPE zlest0047,
        wl_zlest0092 TYPE zlest0092,
        wl_zlest0048 TYPE zlest0048.

  DATA: bool                 TYPE boolean,
        tam                  TYPE i,
        cnpj(14)             TYPE c,
        cnpj_proprietario(8) TYPE c.

  "DATA: PLACA_NUMERO TYPE CHAR4.

  CLEAR: "BOOL,
         tam, wl_zlest0047, wl_zlest0092.


*  "Validar a placa se ela é valida o não.
*  CLEAR: PLACA_NUMERO.
*  PLACA_NUMERO = ZLEST0002-PC_VEICULO(03).
*  "Validar a placa se ela é valida o não.
*  IF ( PLACA_NUMERO(03) CO SY-ABCDE ).
*    BOOL = '-'.
*  ELSE.
*    BOOL = 'X'.
*  ENDIF.
*
*  PLACA_NUMERO = ZLEST0002-PC_VEICULO+03.
*  IF ( PLACA_NUMERO(04) CO '0123456789' ).
*    BOOL = '-'.
*  ELSE.
*    BOOL = 'X'.
*  ENDIF.
*
*  CASE BOOL.
*    WHEN: 'X'.
*      MESSAGE 'Placa precisa ter o formato AAA1111' TYPE 'I'.
*
*  ENDCASE.

  FIND REGEX '[A-Z]{3}[0-9]{1}[A-Z]{1}[0-9]{2}' IN zlest0002-pc_veiculo.
  IF sy-subrc IS NOT INITIAL.
    FIND REGEX '[A-Z]{3}[0-9]{4}' IN zlest0002-pc_veiculo.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Placa precisa ter o formato LLLNLNN ou LLLNNNN, onde L é letra e N é número.' TYPE 'I'.
      erro = 'X'.
    ENDIF.
  ENDIF.


  IF zlest0002-pc_veiculo IS INITIAL.
    MESSAGE 'A placa do veículo deve ser informada!' TYPE 'I'.
    erro = 'X'.
  ENDIF.

  IF zlest0002-proprietario IS INITIAL AND erro IS INITIAL.
    MESSAGE 'O proprietário deve ser informado!' TYPE 'I'.
    erro = 'X'.
  ENDIF.

  zcl_webservice_tipcard=>cons_situacao_transportador(
    EXPORTING
      i_partiner       = zlest0002-proprietario    " Contratado
      i_placa          = zlest0002-pc_veiculo    " Placa veículo
    RECEIVING
      e_consultas      = DATA(e_consultas)   " Tabela de Consultas Transportador
    EXCEPTIONS
      erro             = 1
      webservice       = 2
      OTHERS           = 3 ).

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    erro = 'X'.
  ELSE.
    READ TABLE e_consultas INDEX 1 INTO DATA(wa_consultas).
    IF wa_consultas-ck_rntrc_ativo EQ abap_false AND sy-subrc IS INITIAL.
      sy-msgv1 = wa_consultas-ds_msg_transportador+000(50).
      sy-msgv2 = wa_consultas-ds_msg_transportador+050(50).
      sy-msgv3 = wa_consultas-ds_msg_transportador+100(50).
      sy-msgv4 = wa_consultas-ds_msg_transportador+150(50).
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      erro = 'X'.
    ENDIF.

    IF sy-subrc IS INITIAL.
      IF zlest0002-qt_eixo NE wa_consultas-qt_eixos AND wa_consultas-qt_eixos IS NOT INITIAL.
        MESSAGE w138(zles).
      ENDIF.
    ENDIF.

  ENDIF.

  CALL FUNCTION 'Z_VERIFICA_CLI_FOR_CTA_MAT'
    EXPORTING
      p_koart      = 'K'
      p_fornecedor = zlest0002-proprietario
    EXCEPTIONS
      error        = 1
      OTHERS       = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    erro = 'X'.
  ENDIF.

  IF zlest0002-country IS INITIAL AND erro IS INITIAL.
    MESSAGE 'O país deve ser informado!' TYPE 'I'.
    erro = 'X'.
  ENDIF.

  IF zlest0002-chassi IS INITIAL AND erro IS INITIAL.
    MESSAGE 'O Numero do Chassi deve ser informado!' TYPE 'I'.
    erro = 'X'.
  ENDIF.

  IF zlest0002-cd_renavam IS INITIAL AND erro IS INITIAL.
    MESSAGE 'O Código do Renavam deve ser informado!' TYPE 'I'.
    erro = 'X'.
  ENDIF.

  IF zlest0002-cd_uf IS INITIAL AND erro IS INITIAL.
    MESSAGE 'A Região deve ser informada!' TYPE 'I'.
    erro = 'X'.
  ENDIF.

  IF zlest0002-cto_comodato IS INITIAL AND erro IS INITIAL.
    MESSAGE 'O Campo Contrato de Comodato deve ser informada!' TYPE 'I'.
    erro = 'X'.
  ENDIF.

  IF zlest0002-cto_comodato = '1' AND zlest0002-dt_venc_cto IS INITIAL AND erro IS INITIAL.
    MESSAGE 'O Campo Data do Contrato de Comodato deve ser informada !' TYPE 'I'.
    erro = 'X'.
  ENDIF.

  IF NOT ( zlest0002-marca IS INITIAL ) AND ( erro IS INITIAL ).
    SELECT SINGLE * FROM zlest0047 INTO wl_zlest0047 WHERE marca EQ zlest0002-marca .
    IF ( sy-subrc NE 0 ).
      MESSAGE 'Marca do Veiculo não cadastrada.' TYPE 'I'.
      erro = 'X'.
    ENDIF.
  ENDIF.


  IF NOT ( zlest0002-modelo IS INITIAL )  AND ( erro IS INITIAL ).
    SELECT SINGLE * FROM zlest0092 INTO wl_zlest0092 WHERE modelo EQ zlest0002-modelo.
    IF ( sy-subrc NE 0 ).
      MESSAGE 'Modelo do veiculo não cadastrado.' TYPE 'I'.
      erro = 'X'.
    ENDIF.

  ENDIF.

  IF NOT ( zlest0002-cor IS INITIAL ).
    SELECT SINGLE * FROM zlest0048 INTO wl_zlest0048 WHERE cor EQ zlest0002-cor.
    IF ( sy-subrc NE 0 ).
      MESSAGE 'Cor do veiculo não cadastrada..' TYPE 'I'.
      erro = 'X'.
    ENDIF.
  ENDIF.


  IF NOT ( zlest0002-ct_veiculo IS INITIAL ).
    CASE zlest0002-ct_veiculo.
      WHEN: '1'.
        IF ( zlest0002-tp_veiculo NE '0' ).
          MESSAGE 'O tipo do veículo deve ser 0' TYPE 'I'.
          erro = 'X'.
        ENDIF.
      WHEN: '2'.
        IF ( zlest0002-tp_veiculo NE '1' ).
          MESSAGE 'O tipo do veículo deve ser 1' TYPE 'I'.
          erro = 'X'.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.
  ENDIF.

  IF ( zlest0002-qt_eixo > 4 ).
    MESSAGE 'Quantidade de eixos incorreto.' TYPE 'I'.
    erro = 'X'.
  ENDIF.

  SELECT stcd1 FROM lfa1
  INTO cnpj
WHERE lifnr = zlest0002-proprietario.
  ENDSELECT.

  cnpj_proprietario = cnpj(8).

  IF cnpj_proprietario = '77294254'.

    IF zlest0002-frota IS INITIAL OR zlest0002-grupo IS INITIAL.
      MESSAGE 'O grupo e a Frota devem ser informados' TYPE 'I'.
      erro = 'X'.
    ENDIF.

  ENDIF.


  IF erro IS INITIAL.

    IF vg_2001 EQ 'NOVO'.
      PERFORM carrega_dados_atualizado.
    ENDIF.

    MOVE-CORRESPONDING zlest0002 TO wa_veiculo_tela.
    wa_veiculo_tela-mark = 'X'.

    CLEAR: it_zlest0002[].

    MOVE-CORRESPONDING zlest0002 TO it_zlest0002.

    IF it_zlest0002-erdat EQ ''.
      it_zlest0002-erdat          = sy-datum.
      zlest0002-erdat             = it_zlest0002-erdat.
      it_zlest0002-dt_modificacao = sy-datum.
      zlest0002-dt_modificacao    = sy-datum.
    ELSE.
      it_zlest0002-dt_modificacao = sy-datum.
      zlest0002-dt_modificacao    = sy-datum.

    ENDIF.

    IF it_zlest0002-erzet EQ ''.

      it_zlest0002-erzet           = sy-uzeit.
      zlest0002-erzet              = it_zlest0002-erzet.

      it_zlest0002-usr_modificacao = sy-uname.
      zlest0002-usr_modificacao    = sy-uname.

    ELSE.
      it_zlest0002-usr_modificacao = sy-uname.
      zlest0002-usr_modificacao    = sy-uname.
    ENDIF.

    APPEND it_zlest0002.

    MODIFY zlest0002.

*--> 25.08.2023 16:20:21 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_LES_OUTBOUND_VEICULO' IN BACKGROUND TASK
*      DESTINATION 'XI_VEICULO'
*      AS SEPARATE UNIT
*      TABLES
*        it_veiculo = it_zlest0002.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_LES_OUTBOUND_VEICULO'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          it_veiculo = it_zlest0002.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          it_veiculo = it_zlest0002.
    ENDIF.
*<-- 25.08.2023 16:20:21 - Migração S4 – ML – Fim

    COMMIT WORK.

    "Salvar modificações.
    PERFORM fm_salve_log USING zlest0002.

    CLEAR: vg_alterou, vg_pesquis.
    vg_2001 = 'CONS'.



  ENDIF.


ENDFORM.                    " SALVAR

*&---------------------------------------------------------------------*
*&      Form  SAIR
*&---------------------------------------------------------------------*
*       text
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
      WHEN 'N'.
        CLEAR: vg_alterou, vg_pesquis.
      WHEN 'A'.
        CLEAR vg_pesquis.
        EXIT.
    ENDCASE.

  ENDIF.
ENDFORM.                    " SAIR

*&---------------------------------------------------------------------*
*&      Module  F_D0100_TAXJURCODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_d0100_taxjurcode INPUT.

  DATA: ti_help         TYPE STANDARD TABLE OF ty_help_domicilio INITIAL SIZE 0 WITH HEADER LINE.

  DATA: l_kalsm         TYPE ttxd-kalsm,
        l_country       TYPE land1_gp,
        l_estado        TYPE regio,
        l_cep           TYPE pstlz,

        lo_jurisdiction TYPE REF TO cl_tax_jurisdiction_code,
        vg_langu        TYPE sy-langu.

  DATA: BEGIN OF dynpfields OCCURS 2.
          INCLUDE STRUCTURE dynpread.
  DATA: END   OF dynpfields.

  CHECK NOT vg_pesquis IS INITIAL.

  dynpfields-fieldname = 'ZLEST0002-COUNTRY'.
  APPEND dynpfields.

  dynpfields-fieldname = 'ZLEST0002-CD_UF'.
  APPEND dynpfields.

  dynpfields-fieldname = 'ZLEST0002-PSTLZ'.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = 'SAPMZLESVEIC'
      dynumb               = '2001'
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

  READ TABLE dynpfields WITH KEY fieldname = 'ZLEST0002-COUNTRY'.
  l_country = dynpfields-fieldvalue.

  READ TABLE dynpfields WITH KEY fieldname = 'ZLEST0002-CD_UF'.
  l_estado = dynpfields-fieldvalue.

  READ TABLE dynpfields WITH KEY fieldname = 'ZLEST0002-PSTLZ'.
  l_cep = dynpfields-fieldvalue.



  IF l_country IS INITIAL.
    MESSAGE 'Favor informar o País!' TYPE 'W'.
  ENDIF.

  CALL METHOD cl_tax_jurisdiction_code=>get_kalsm
    EXPORTING
      im_country       = l_country
    RECEIVING
      re_kalsm         = l_kalsm
    EXCEPTIONS
      input_incomplete = 1
      no_tax_procedure = 2
      OTHERS           = 3.

  IF sy-subrc <> 0.
    MESSAGE 'Não foi encontrado esquema de cálculo!' TYPE 'S'.
    STOP.
  ENDIF.

  IF l_estado IS INITIAL.

    IF l_cep IS INITIAL.
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
         AND cep~pstcd_from <= l_cep
         AND cep~pstcd_to   >= l_cep.
    ENDIF.

  ELSE.
    IF l_cep IS INITIAL.

      SELECT cep~country
             cep~region
             cep~taxjurcode
             cep~pstcd_from
             cid~text
        INTO CORRESPONDING FIELDS OF TABLE ti_help
        FROM j_1btreg_city AS cep
        INNER JOIN j_1btxjurt AS cid ON cid~country EQ cep~country
                                 AND cid~taxjurcode EQ cep~taxjurcode
       WHERE cep~country EQ l_country
         AND cep~region  EQ l_estado .

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
        AND cep~region  EQ l_estado
        AND cep~pstcd_from <= l_cep
        AND cep~pstcd_to   >= l_cep.

    ENDIF.

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

  MOVE: 'ZLEST0002-COUNTRY'    TO  t_dynpfields-fieldname,
        ti_help-country        TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZLEST0002-CD_UF'      TO  t_dynpfields-fieldname,
        ti_help-region         TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZLEST0002-CD_CIDADE'  TO  t_dynpfields-fieldname,
        ti_help-text           TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZLEST0002-TAXJURCODE' TO  t_dynpfields-fieldname,
        ti_help-taxjurcode     TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZLEST0002-KALSM'      TO  t_dynpfields-fieldname,
        l_kalsm                TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZLEST0002-PSTLZ'       TO t_dynpfields-fieldname,
         ti_help-pstcd_from     TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.


  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
    EXPORTING
      input  = sy-langu
    IMPORTING
      output = vg_langu.

  MOVE: 'ZLEST0002-SPRAS'      TO  t_dynpfields-fieldname,
        vg_langu               TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.


  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.


ENDMODULE.                 " F_D0100_TAXJURCODE  INPUT
*&---------------------------------------------------------------------*
*&      Module  F_MODELO  INPUT
*&---------------------------------------------------------------------*
MODULE f_modelo INPUT.

  DATA: ti_help_modelo  TYPE STANDARD TABLE OF ty_help_modelo INITIAL SIZE 0 WITH HEADER LINE.
  DATA: l_marca TYPE zmarca_veic.

  DATA: BEGIN OF dynpfields_modelo OCCURS 2.
          INCLUDE STRUCTURE dynpread.
  DATA: END   OF dynpfields_modelo.

  dynpfields_modelo-fieldname = 'ZLEST0002-MARCA'.
  APPEND dynpfields_modelo.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = 'SAPMZLESVEIC'
      dynumb               = '2001'
    TABLES
      dynpfields           = dynpfields_modelo
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

  READ TABLE dynpfields_modelo WITH KEY fieldname = 'ZLEST0002-MARCA'.
  l_marca = dynpfields_modelo-fieldvalue.

  IF NOT ( l_marca IS INITIAL ).
    SELECT marca modelo
      FROM zlest0092
      INTO TABLE ti_help_modelo
    WHERE marca EQ l_marca.
  ENDIF.

  IF NOT ( ti_help_modelo[] IS INITIAL ).

    SORT: ti_help_modelo BY modelo.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield   = 'MODELO'
        dynpprog   = sy-repid
        dynpnr     = sy-dynnr
        value_org  = 'S'
      TABLES
        value_tab  = ti_help_modelo[]
        return_tab = t_ret.


    READ TABLE t_ret INTO st_ret INDEX 1.
    CHECK sy-subrc IS INITIAL.

    READ TABLE ti_help_modelo WITH KEY modelo = st_ret-fieldval BINARY SEARCH.

    MOVE: 'ZLEST0002-MODELO'    TO  t_dynpfields-fieldname,
          ti_help_modelo-modelo TO  t_dynpfields-fieldvalue.
    APPEND t_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = t_dynpfields.
  ELSE.
  ENDIF.
ENDMODULE.                 " F_MODELO  INPUT


*----------------------------------------------------------------------*
*  MODULE f_d0100_taxjurcode2 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE f_d0100_taxjurcode2 INPUT.
  PERFORM d0100_taxjurcode
          USING zlest0002-country
                sy-dynnr
       CHANGING zlest0002-taxjurcode.

ENDMODULE.                 " F_D0100_TAXJURCODE  INPUT



*&---------------------------------------------------------------------*
*&      Form  d0100_taxjurcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_COUNTRY        text
*      -->P_DYNPRO_NUMBER  text
*      -->P_TAXJURCODE     text
*----------------------------------------------------------------------*
FORM d0100_taxjurcode
   USING    p_country
            p_dynpro_number
   CHANGING p_taxjurcode.

  DATA: f4_display TYPE c LENGTH 1.

  "IF g_dialog_mode = display OR addr1_fsel+47(1) EQ '*'.
  "  f4_display = c_yes.
  "ELSE.
  f4_display = ''.
  " ENDIF.
*891i+
  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      funcname           = 'TAX_TXJCD_HELP_VALUE'
    EXCEPTIONS
      function_not_exist = 1
      OTHERS             = 2.

  IF sy-subrc = 0.
    CALL FUNCTION 'TAX_TXJCD_HELP_VALUE'
      EXPORTING
        dynpro_field_country    = 'ZLEST0002-COUNTRY'
        dynpro_field_region     = 'ZLEST0002-CD_UF'
        dynpro_field_zipcode    = 'ZLEST0002-PSTLZ'
        dynpro_field_city       = 'ZLEST0002-CD_CIDADE'
        "dynpro_field_county     = 'ZLEST0002-CITY2'
        dynpro_field_taxjurcode = 'ZLEST0002-TAXJURCODE'
        dynpro_number           = p_dynpro_number
        dynpro_name             = 'SAPLSZA1'
        screen_input            = f4_display
*       dynpro_field_country    = 'ADDR1_DATA-COUNTRY'
*       dynpro_field_region     = 'ADDR1_DATA-REGION'
*       dynpro_field_zipcode    = 'ADDR1_DATA-POST_CODE1'
*       dynpro_field_city       = 'ADDR1_DATA-CITY1'
*       dynpro_field_county     = 'ADDR1_DATA-CITY2'
*       dynpro_field_taxjurcode = 'ADDR1_DATA-TAXJURCODE'
*       dynpro_number           = p_dynpro_number
*       dynpro_name             = 'SAPLSZA1'
*       screen_input            = f4_display
      IMPORTING
        taxjurcode              = p_taxjurcode
      EXCEPTIONS
        OTHERS                  = 1.

    "IF sy-subrc <> 0.
    "  MESSAGE s885 WITH 'TAX_TXJCD_HELP_VALUE'.
    "ENDIF.
  ELSE.
*891i-
*-------------------------------------------------- "*249i+
*{                            Prüfung des Tax Jurisdiction Code
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = 'JURISDICTION_HELP_VALUE'
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc = 0.
* call of JURISDICTION_HELP_VALUE is possible
*-------------------------------------------------- "*249i-

      CALL FUNCTION 'JURISDICTION_HELP_VALUE'
        EXPORTING
          dynpro_field_country    = 'ADDR1_DATA-COUNTRY'
          dynpro_field_taxjurcode = 'ADDR1_DATA-TAXJURCODE'
          dynpro_number           = p_dynpro_number
          dynpro_name             = 'SAPLSZA1'
          screen_input            = f4_display
        IMPORTING
          taxjurcode              = p_taxjurcode
        EXCEPTIONS
          OTHERS                  = 1.
      "IF sy-subrc <> 0.
      "  MESSAGE s885 WITH 'JURISDICTION_HELP_VALUE'.
      "ENDIF.
    ELSE.                                                   "*249i
      "  MESSAGE s821.                                         "*249i
    ENDIF.                                                  "*249i
  ENDIF.                                                    "*891i

ENDFORM.                    "d0100_taxjurcode


*&---------------------------------------------------------------------*
*&      Form  Teste
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM teste.
  DATA : esdus_stream                  TYPE REF TO cl_esdus_connector_mm_2,
         memory_stream                 TYPE REF TO cl_memory_connector_mm,
         personalization_arbeitsvorrat TYPE REF TO cl_personalization_mm,
         framework                     TYPE REF TO cl_framework_mm,
         personalization               TYPE REF TO cl_personalization_mm,
         env_functions                 TYPE REF TO cl_environment_functions_mm.

  CALL METHOD cl_framework_mm=>get_instance
    IMPORTING
      ex_instance = framework.
  env_functions = cl_environment_functions_mm=>get_instance( ).

  CREATE OBJECT esdus_stream
    EXPORTING
      im_action_id = 'MEPO'.                                "#EC NOTEXT

  CREATE OBJECT memory_stream
    EXPORTING
      im_action_id = 'DATABLADE'                            "#EC NOTEXT
      im_memory_id = 'MEPODATABLADE'.                       "#EC NOTEXT

  CREATE OBJECT: personalization_arbeitsvorrat.

  personalization = framework->get_personalization( ).

  CALL METHOD personalization->append(
      im_name   = 'Arbeitsvorrat'            "#EC NOTEXT
      im_object = personalization_arbeitsvorrat ).

ENDFORM.                    "Teste
*&---------------------------------------------------------------------*
*&      Module  BUSCA_NUMERO_EQUIP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_numero_equip INPUT.

  IF zlest0002-pc_veiculo IS NOT INITIAL OR wa_veiculo_tela-pc_veiculo IS NOT INITIAL.
    "Seleciona o equipamento e centro do equipamento.
    CLEAR: zlest0002-equnr, zlest0002-werks, wa_veiculo_tela-equnr, wa_veiculo_tela-werks.
    SELECT SINGLE c~iwerk c~equnr
    FROM fleet AS a
    INNER JOIN equi AS b ON b~objnr EQ a~objnr
    INNER JOIN equz AS c ON c~equnr EQ b~equnr
      INTO ( wa_veiculo_tela-werks, wa_veiculo_tela-equnr )
        WHERE a~license_num EQ zlest0002-pc_veiculo
          AND b~eqtyp EQ 'A'
          AND c~datbi EQ '99991231'.

    zlest0002-equnr = wa_veiculo_tela-equnr.
    zlest0002-werks = wa_veiculo_tela-werks.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TRATA_COMODATO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE trata_comodato INPUT.

  IF zlest0002-cto_comodato = 'SIM' OR zlest0002-cto_comodato = '1'.

    LOOP AT SCREEN.

      IF screen-name =  'ZLEST0002-PROPRIET_COMODATO'.

        screen-input = 1."habilitado
        MODIFY SCREEN.

      ENDIF.


    ENDLOOP.


  ELSE.

    LOOP AT SCREEN.

      IF screen-name =  'ZLEST0002-PROPRIET_COMODATO'.

        screen-input = 0."deshabilitado
        MODIFY SCREEN.

      ENDIF.


    ENDLOOP.

  ENDIF.
ENDMODULE.
