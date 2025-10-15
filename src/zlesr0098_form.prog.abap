*&---------------------------------------------------------------------*
*&  Include           ZLESR0098_FORM
*&---------------------------------------------------------------------*

FORM f_selecionar_dados_classif.

  FREE: tg_zsdt0001,
        tg_zsdt0001_rem_s,
        tg_zsdt0001_rem_e,
        tg_zlest0039,
        it_saida_0200[].

  DATA: msg_exibir TYPE string.

  PERFORM f_iniciar_variaveis.

  SELECT *
    FROM zsdt0001
    INTO CORRESPONDING FIELDS OF TABLE tg_zsdt0001
   WHERE bukrs            IN  p_bukrs
     AND branch           IN  p_branch
     AND id_cli_dest      IN  p_id_cli
     AND dt_movimento     IN  p_dt_mov
     AND matnr            IN  p_matnr
     AND nr_safra         IN  p_safra
     AND tp_movimento      = 'S'.

  IF tg_zsdt0001[] IS INITIAL.
    MESSAGE 'Nenhum movimento encontrado!' TYPE 'S'.
    EXIT.
  ENDIF.

  tg_zsdt0001_aux[] = tg_zsdt0001[].

  DELETE tg_zsdt0001_aux WHERE doc_rem IS INITIAL.

  SORT tg_zsdt0001_aux BY doc_rem.
  DELETE ADJACENT DUPLICATES FROM tg_zsdt0001_aux
                        COMPARING doc_rem.

  IF tg_zsdt0001_aux[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0001
      INTO TABLE tg_zsdt0001_rem_e
       FOR ALL ENTRIES IN tg_zsdt0001_aux
     WHERE tp_movimento = 'E'
       AND doc_rem      = tg_zsdt0001_aux-doc_rem.

    SELECT *
      FROM zlest0039
      INTO TABLE tg_zlest0039
       FOR ALL ENTRIES IN tg_zsdt0001_aux
     WHERE vbeln  = tg_zsdt0001_aux-doc_rem.
  ENDIF.

  LOOP AT tg_zsdt0001.
    DATA(l_tabix) = sy-tabix.

    tg_zsdt0001-numero  = tg_zsdt0001-nfnum.
    tg_zsdt0001-serie   = tg_zsdt0001-series.
    tg_zsdt0001-msg_v1  = tg_zsdt0001-ch_referencia.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = tg_zsdt0001-serie
      IMPORTING
        output = tg_zsdt0001-serie.

    MODIFY tg_zsdt0001 INDEX l_tabix.
  ENDLOOP.

* SELECT *
*   FROM zsdt0001
*   INTO TABLE tg_zsdt0001_rem_s
*    FOR ALL ENTRIES IN tg_zsdt0001_aux
*  WHERE doc_rem      = tg_zsdt0001_aux-doc_rem
*    AND tp_movimento = 'S'.

* SORT tg_zsdt0001_rem_s BY doc_rem.
* DELETE ADJACENT DUPLICATES FROM tg_zsdt0001_rem_s
*                       COMPARING doc_rem.

  SORT tg_zsdt0001_rem_e BY doc_rem.
  DELETE ADJACENT DUPLICATES FROM tg_zsdt0001_rem_e
                        COMPARING doc_rem.

  SORT tg_zlest0039 BY vbeln.
  DELETE ADJACENT DUPLICATES FROM tg_zlest0039
                        COMPARING vbeln.

  "Status Reenvio SAP
  SELECT *
    FROM zob_ret_msg
    INTO TABLE tg_zob_ret_msg
     FOR ALL ENTRIES IN tg_zsdt0001
   WHERE msg_v1      EQ tg_zsdt0001-msg_v1
     AND cd_processo EQ '001'. "Romaneio

  "Pegar Ultimos registros
  SORT tg_zob_ret_msg BY msg_v1 dt_registro hr_registro DESCENDING.
  DELETE ADJACENT DUPLICATES FROM tg_zob_ret_msg COMPARING msg_v1.

ENDFORM.


*&---------------------------------------------------------------------*
*&  Include           ZLESR0098_FORM
*&---------------------------------------------------------------------*

FORM f_selecionar_dados .

  CLEAR: tg_zsdt0001[], tg_lfa1[], tg_kna1[], tg_makt[], tg_vbfa[],
         tg_j_1bnflin[], tg_j_1bnfe_active[], tg_zib_nfe_dist_ter[],
         it_saida_0100[], tg_zob_ret_msg[].

  DATA: msg_exibir TYPE string,
        lr_tp_mov  TYPE RANGE OF zsdt0001-tp_movimento.

  PERFORM f_iniciar_variaveis.

  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
     ID 'WERKS' FIELD  p_branch-low
     ID 'ACTVT' FIELD '03'.

  IF sy-subrc IS NOT INITIAL.
    CONCATENATE 'Usuário sem permissão para o centro:' p_branch-low
            INTO msg_exibir SEPARATED BY space.

    MESSAGE msg_exibir  TYPE 'S'.
    SET CURSOR FIELD 'P_BRANCH-LOW'.
    EXIT.
  ENDIF.

*** Inicio - Rubenilson - 17.09.24 - #151332
  CASE abap_true.
    WHEN p_entr.
      APPEND INITIAL LINE TO lr_tp_mov ASSIGNING FIELD-SYMBOL(<fs_tp_mov>).

      <fs_tp_mov>-sign = 'I'.
      <fs_tp_mov>-option = 'EQ'.
      <fs_tp_mov>-low = 'E'.
    WHEN p_said.
      APPEND INITIAL LINE TO lr_tp_mov ASSIGNING <fs_tp_mov>.

      <fs_tp_mov>-sign = 'I'.
      <fs_tp_mov>-option = 'EQ'.
      <fs_tp_mov>-low = 'S'.
    WHEN OTHERS.
  ENDCASE.
*** Fim - Rubenilson - 17.09.24 - #151332

  SELECT *
    FROM zsdt0001 INTO CORRESPONDING FIELDS OF TABLE tg_zsdt0001
   WHERE ch_referencia    IN  p_ch_ref
     AND bukrs            IN  p_bukrs
     AND branch           IN  p_branch
     AND dt_movimento     IN  p_dt_mov
     AND tp_movimento     IN  lr_tp_mov "Rubenilson - 17.09.24 - #151332
     AND parid            IN  p_parid
     AND nr_romaneio      IN  p_num_ro
     AND nr_safra         IN  p_safra
     AND tp_frete         IN  p_tp_fre
     AND matnr            IN  p_matnr
     AND motorista        IN  p_moto
     AND dt_fechamento    IN  p_dt_fec
     AND dt_abertura      IN  p_dt_abe
     AND doc_rem          IN  p_dc_rem
     AND tp_transgenia    IN  p_transg
     AND local_descarga   IN  p_lc_des
     AND tipo_entrada     IN  p_tp_ent
     AND ( ( placa_cav    IN  p_placa ) OR
           ( placa_car1   IN  p_placa ) OR
           ( placa_car2   IN  p_placa ) OR
           ( placa_car3   IN  p_placa ) ).

  IF tg_zsdt0001[] IS INITIAL.
    MESSAGE 'Nenhum movimento encontrado!' TYPE 'S'.
    EXIT.
  ENDIF.

  "Dados Fornecedor
  tg_zsdt0001_aux[] = tg_zsdt0001[].
  SORT tg_zsdt0001_aux BY parid.
  DELETE ADJACENT DUPLICATES FROM tg_zsdt0001_aux COMPARING parid.

  SELECT lifnr name1 stcd1
    FROM lfa1 INTO CORRESPONDING FIELDS OF TABLE tg_lfa1
    FOR ALL ENTRIES IN tg_zsdt0001_aux
   WHERE lifnr = tg_zsdt0001_aux-parid.

  "Dados Clientes
  SELECT kunnr name1 stcd1
    FROM kna1 INTO CORRESPONDING FIELDS OF TABLE tg_kna1
    FOR ALL ENTRIES IN tg_zsdt0001_aux
   WHERE kunnr = tg_zsdt0001_aux-parid.

  "Dados Material
  tg_zsdt0001_aux[] = tg_zsdt0001[].
  SORT tg_zsdt0001_aux BY matnr.
  DELETE ADJACENT DUPLICATES FROM tg_zsdt0001_aux COMPARING matnr.

  SELECT matnr maktx
    FROM makt INTO CORRESPONDING FIELDS OF TABLE tg_makt
    FOR ALL ENTRIES IN tg_zsdt0001_aux
   WHERE spras = sy-langu
     AND matnr = tg_zsdt0001_aux-matnr.

  IF p_chv_nf IS NOT INITIAL.
    "Dados Fatura
    SELECT vbelv vbtyp_n vbeln vbeln
      FROM vbfa INTO TABLE tg_vbfa
       FOR ALL ENTRIES IN tg_zsdt0001
     WHERE vbelv   EQ tg_zsdt0001-doc_rem
       AND vbtyp_n EQ 'M'.

    IF tg_vbfa[] IS NOT INITIAL.
      LOOP AT tg_vbfa WHERE vbeln_refkey IS INITIAL.
        tg_vbfa-vbeln_refkey = tg_vbfa-vbeln.
        MODIFY tg_vbfa.
      ENDLOOP.

      SELECT docnum refkey
        FROM j_1bnflin INTO CORRESPONDING FIELDS OF TABLE tg_j_1bnflin
         FOR ALL ENTRIES IN tg_vbfa
       WHERE refkey EQ tg_vbfa-vbeln_refkey.

      SELECT docnum regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv
        FROM j_1bnfe_active INTO CORRESPONDING FIELDS OF TABLE tg_j_1bnfe_active
         FOR ALL ENTRIES IN tg_j_1bnflin
       WHERE docnum EQ tg_j_1bnflin-docnum
         AND cancel NE 'X'.
    ENDIF.
  ENDIF.

  "Dados Operação Romaneio
  tg_zsdt0001_aux[] = tg_zsdt0001[].
  SORT tg_zsdt0001_aux BY tipo_entrada.
  DELETE ADJACENT DUPLICATES FROM tg_zsdt0001_aux COMPARING tipo_entrada.

  SELECT *
    FROM zlest0129 INTO CORRESPONDING FIELDS OF TABLE tg_zlest0129
     FOR ALL ENTRIES IN tg_zsdt0001_aux
   WHERE tipo_entrada EQ tg_zsdt0001_aux-tipo_entrada.

  SORT: tg_lfa1            BY lifnr,
        tg_kna1            BY kunnr,
        tg_makt            BY matnr,
        tg_vbfa            BY vbelv,
        tg_j_1bnflin       BY refkey,
        tg_j_1bnfe_active  BY docnum,
        tg_zlest0129       BY tipo_entrada,
        tg_zob_ret_msg     BY msg_v1.

  LOOP AT tg_zsdt0001.
    CASE tg_zsdt0001-tp_movimento.
      WHEN 'E'.
        READ TABLE tg_lfa1 WITH KEY lifnr = tg_zsdt0001-parid BINARY SEARCH.
        IF sy-subrc = 0.
          tg_zsdt0001-forne_cnpj = tg_lfa1-stcd1.
        ENDIF.
      WHEN 'S'.
        READ TABLE tg_kna1 WITH KEY kunnr = tg_zsdt0001-parid BINARY SEARCH.
        IF sy-subrc = 0.
          tg_zsdt0001-forne_cnpj = tg_kna1-stcd1.
        ENDIF.
    ENDCASE.
    tg_zsdt0001-numero  = tg_zsdt0001-nfnum.
    tg_zsdt0001-serie   = tg_zsdt0001-series.
    tg_zsdt0001-msg_v1  = tg_zsdt0001-ch_referencia.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = tg_zsdt0001-serie
      IMPORTING
        output = tg_zsdt0001-serie.

    MODIFY tg_zsdt0001.
  ENDLOOP.

  IF p_chv_nf IS NOT INITIAL.
    SELECT forne_cnpj numero serie chave_nfe
      FROM zib_nfe_dist_ter INTO CORRESPONDING FIELDS OF TABLE tg_zib_nfe_dist_ter
       FOR ALL ENTRIES IN tg_zsdt0001
     WHERE forne_cnpj = tg_zsdt0001-forne_cnpj
       AND numero     = tg_zsdt0001-numero
       AND serie      = tg_zsdt0001-serie.

    SORT: tg_zib_nfe_dist_ter BY forne_cnpj numero serie.
  ENDIF.

  "Status Reenvio SAP
  SELECT *
    FROM zob_ret_msg INTO TABLE tg_zob_ret_msg
     FOR ALL ENTRIES IN tg_zsdt0001
   WHERE msg_v1      EQ tg_zsdt0001-msg_v1
     AND cd_processo EQ '001'. "Romaneio

  "Pegar Ultimos registros
  SORT tg_zob_ret_msg BY msg_v1 dt_registro hr_registro DESCENDING.
  DELETE ADJACENT DUPLICATES FROM tg_zob_ret_msg COMPARING msg_v1.

ENDFORM.

FORM f_iniciar_variaveis .

  DATA: wl_butxt(50)   TYPE c,
        wl_name(50)    TYPE c,

        wl_bukrs(100)  TYPE c,
        wl_branch(50)  TYPE c,
        wl_dt_mov(50)  TYPE c,
        wl_safra(50)   TYPE c,

        wl_layout1(20) VALUE 'Empresa:',
        wl_layout2(20) VALUE 'Centro:',
        wl_layout3(20) VALUE 'Período: ',
        wl_layout4(20) VALUE 'Safra:',

        wl_data        VALUE '.',
        wl_space       VALUE '-',
        wl_ate(3)      VALUE 'até'.

  CLEAR: tg_zsdt0001[],
         it_saida_0100[],
         tg_zlest0129[],
         tg_j_1bnfe_active[],
         tg_j_1bnflin[],
         tg_makt[],
         tg_vbfa[],
         tg_lfa1[],
         tg_kna1[],
         t_top[].

  SELECT SINGLE butxt
    FROM t001 INTO wl_butxt
   WHERE bukrs IN p_bukrs.

  SELECT SINGLE name
    FROM j_1bbranch INTO wl_name
   WHERE bukrs  IN p_bukrs
     AND branch IN p_branch.


  CONCATENATE wl_layout1 p_bukrs+3(4) wl_space wl_butxt  INTO wl_bukrs SEPARATED BY space.  "Empresa
  CONCATENATE wl_layout2 p_branch+3(4) wl_space wl_name  INTO wl_branch SEPARATED BY space. "Filial

  IF p_dt_mov-low  IS NOT INITIAL AND
     p_dt_mov-high IS NOT INITIAL.
    CONCATENATE wl_layout3 p_dt_mov-low  wl_ate p_dt_mov-high INTO  wl_dt_mov SEPARATED BY space.
  ENDIF.

  IF p_dt_mov-low  IS NOT INITIAL AND
     p_dt_mov-high IS INITIAL.
    CONCATENATE wl_layout3 p_dt_mov-low INTO  wl_dt_mov SEPARATED BY space.
  ENDIF.

  IF p_safra-low IS NOT INITIAL.
    CONCATENATE wl_layout4 p_safra-low INTO  wl_safra SEPARATED BY space.
  ENDIF.

  IF p_romane = abap_true.
    PERFORM f_construir_cabecalho USING 'H' TEXT-001.
  ELSE.
    PERFORM f_construir_cabecalho USING 'H' TEXT-004.
  ENDIF.

  PERFORM f_construir_cabecalho USING 'S' wl_bukrs.
  PERFORM f_construir_cabecalho USING 'S' wl_branch.

  IF wl_dt_mov IS NOT INITIAL.
    PERFORM f_construir_cabecalho USING 'S' wl_dt_mov.
  ENDIF.

  IF wl_safra IS NOT INITIAL.
    PERFORM f_construir_cabecalho USING 'S' wl_safra.
  ENDIF.

  v_report = sy-repid.
  gs_variant-report      = sy-repid.


ENDFORM.

FORM f_processar_dados_classif.

  LOOP AT tg_zsdt0001.

    CLEAR: wa_saida_0200,
           ws_zsdt0001_rem_s,
           ws_zsdt0001_rem_e,
           ws_zlest0039.

    READ TABLE tg_zsdt0001_rem_e INTO ws_zsdt0001_rem_e
                                 WITH KEY doc_rem = tg_zsdt0001-doc_rem
                                 BINARY SEARCH.
    READ TABLE tg_zlest0039      INTO ws_zlest0039
                                 WITH KEY vbeln   = tg_zsdt0001-doc_rem
                                 BINARY SEARCH.

    MOVE-CORRESPONDING tg_zsdt0001            TO wa_saida_0200.
    MOVE-CORRESPONDING ws_zlest0039           TO wa_saida_0200.
    MOVE icon_light_out                       TO wa_saida_0200-icon_reenvio.
*
    MOVE tg_zsdt0001-nr_perc_umidade          TO wa_saida_0200-nr_perc_umidade_s.
    MOVE tg_zsdt0001-nr_qtd_umidade           TO wa_saida_0200-nr_qtd_umidade_s.
    MOVE tg_zsdt0001-nr_perc_impureza         TO wa_saida_0200-nr_perc_impureza_s.
    MOVE tg_zsdt0001-nr_qtd_impureza          TO wa_saida_0200-nr_qtd_impureza_s.
    MOVE tg_zsdt0001-nr_perc_avaria           TO wa_saida_0200-nr_perc_avaria_s.
    MOVE tg_zsdt0001-nr_qtd_avaria            TO wa_saida_0200-nr_qtd_avaria_s.
    MOVE tg_zsdt0001-nr_perc_ardido           TO wa_saida_0200-nr_perc_ardido_s.
    MOVE tg_zsdt0001-nr_qtd_ardido            TO wa_saida_0200-nr_qtd_ardido_s.
    MOVE tg_zsdt0001-nr_perc_quebra           TO wa_saida_0200-nr_perc_quebra_s.
    MOVE tg_zsdt0001-nr_qtd_quebra            TO wa_saida_0200-nr_qtd_quebra_s.
    MOVE tg_zsdt0001-nr_perc_esverd           TO wa_saida_0200-nr_perc_esverd_s.
    MOVE tg_zsdt0001-nr_qtd_esverd            TO wa_saida_0200-nr_qtd_esverd_s.
*
    MOVE ws_zsdt0001_rem_e-nr_perc_umidade    TO wa_saida_0200-nr_perc_umidade_e.
    MOVE ws_zsdt0001_rem_e-nr_qtd_umidade     TO wa_saida_0200-nr_qtd_umidade_e.
    MOVE ws_zsdt0001_rem_e-nr_perc_impureza   TO wa_saida_0200-nr_perc_impureza_e.
    MOVE ws_zsdt0001_rem_e-nr_qtd_impureza    TO wa_saida_0200-nr_qtd_impureza_e.
    MOVE ws_zsdt0001_rem_e-nr_perc_avaria     TO wa_saida_0200-nr_perc_avaria_e.
    MOVE ws_zsdt0001_rem_e-nr_qtd_avaria      TO wa_saida_0200-nr_qtd_avaria_e.
    MOVE ws_zsdt0001_rem_e-nr_perc_ardido     TO wa_saida_0200-nr_perc_ardido_e.
    MOVE ws_zsdt0001_rem_e-nr_qtd_ardido      TO wa_saida_0200-nr_qtd_ardido_e.
    MOVE ws_zsdt0001_rem_e-nr_perc_quebra     TO wa_saida_0200-nr_perc_quebra_e.
    MOVE ws_zsdt0001_rem_e-nr_qtd_quebra      TO wa_saida_0200-nr_qtd_quebra_e.
    MOVE ws_zsdt0001_rem_e-nr_perc_esverd     TO wa_saida_0200-nr_perc_esverd_e.
    MOVE ws_zsdt0001_rem_e-nr_qtd_esverd      TO wa_saida_0200-nr_qtd_esverd_e.

    "Verifica Status Reenvio SAP
    READ TABLE tg_zob_ret_msg WITH KEY msg_v1 = wa_saida_0200-ch_referencia.
    IF sy-subrc = 0.
      IF tg_zob_ret_msg-rg_atualizado = 'N'.
        wa_saida_0200-icon_reenvio = icon_yellow_light.
      ELSE.
        wa_saida_0200-icon_reenvio = icon_green_light.
      ENDIF.
    ENDIF.

*"// WBARBOSA 16102024 Verifica se Atende EUDR US154984
    IF ws_zsdt0001_rem_e-eudr EQ gc_atende_eudr.
      wa_saida_0200-icon_eudr = icon_checked.
    ELSE.
      CLEAR wa_saida_0200-icon_eudr.
    ENDIF.
*"// WBARBOSA 16102024 Verifica se Atende EUDR US154984

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_saida_0200-parid
      IMPORTING
        output = wa_saida_0200-parid.

    APPEND wa_saida_0200                      TO it_saida_0200.
  ENDLOOP.

ENDFORM.

FORM f_processar_dados .

  LOOP AT tg_zsdt0001.

    CLEAR: wa_saida_0100, tg_makt, tg_lfa1, tg_kna1.

    wa_saida_0100-ic_xml                =  icon_led_red.
    wa_saida_0100-ch_referencia         =  tg_zsdt0001-ch_referencia.
    wa_saida_0100-tp_movimento          =  tg_zsdt0001-tp_movimento.

    CASE wa_saida_0100-tp_movimento.
      WHEN 'E'.
        wa_saida_0100-ic_mov = icon_incoming_object.
      WHEN 'S'.
        wa_saida_0100-ic_mov = icon_outgoing_object.
    ENDCASE.

    wa_saida_0100-nr_romaneio           =  tg_zsdt0001-nr_romaneio.
    wa_saida_0100-cfop                  =  tg_zsdt0001-cfop.
    wa_saida_0100-vbeln                 =  tg_zsdt0001-vbeln.
    wa_saida_0100-dt_movimento          =  tg_zsdt0001-dt_movimento.
    wa_saida_0100-nr_safra              =  tg_zsdt0001-nr_safra.
    wa_saida_0100-bukrs                 =  tg_zsdt0001-bukrs.
    wa_saida_0100-branch                =  tg_zsdt0001-branch.
    wa_saida_0100-parid                 =  tg_zsdt0001-parid.
    wa_saida_0100-st_cct                =  tg_zsdt0001-st_cct.
    wa_saida_0100-icon_reenvio          =  icon_light_out.

    "Verifica Status Reenvio SAP
    READ TABLE tg_zob_ret_msg WITH KEY msg_v1 = wa_saida_0100-ch_referencia.
    IF sy-subrc = 0.
      IF tg_zob_ret_msg-rg_atualizado = 'N'.
        wa_saida_0100-icon_reenvio = icon_yellow_light.
      ELSE.
        wa_saida_0100-icon_reenvio = icon_green_light.
      ENDIF.
    ENDIF.

*"// WBARBOSA 16102024 Verifica se Atende EUDR US154984
    IF tg_zsdt0001-eudr EQ gc_atende_eudr.
      wa_saida_0100-icon_eudr = icon_checked.
    ELSE.
      CLEAR wa_saida_0100-icon_eudr.
    ENDIF.
*"// WBARBOSA 16102024 Verifica se Atende EUDR US154984

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_saida_0100-parid
      IMPORTING
        output = wa_saida_0100-parid.

    IF wa_saida_0100-parid IS NOT INITIAL.
      CASE wa_saida_0100-tp_movimento.
        WHEN 'E'.

          READ TABLE tg_lfa1 WITH KEY lifnr = wa_saida_0100-parid BINARY SEARCH.
          IF sy-subrc = 0.
            wa_saida_0100-name1 = tg_lfa1-name1.
            wa_saida_0100-stcd1 = tg_lfa1-stcd1.
          ENDIF.
        WHEN 'S'.
          READ TABLE tg_kna1 WITH KEY kunnr = wa_saida_0100-parid BINARY SEARCH.
          IF sy-subrc = 0.
            wa_saida_0100-name1 = tg_kna1-name1.
            wa_saida_0100-stcd1 = tg_kna1-stcd1.
          ENDIF.
      ENDCASE.
    ENDIF.

    wa_saida_0100-tp_frete              =  tg_zsdt0001-tp_frete.
    wa_saida_0100-matnr                 =  tg_zsdt0001-matnr.

    IF wa_saida_0100-matnr IS NOT INITIAL.
      READ TABLE tg_makt WITH KEY matnr = wa_saida_0100-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida_0100-maktx = tg_makt-maktx.
      ENDIF.
    ENDIF.

    wa_saida_0100-peso_liq              =  tg_zsdt0001-peso_liq.
    wa_saida_0100-peso_fiscal           =  tg_zsdt0001-peso_fiscal.
    wa_saida_0100-nfnum                 =  tg_zsdt0001-nfnum.
    wa_saida_0100-series                =  tg_zsdt0001-series.
    wa_saida_0100-docdat                =  tg_zsdt0001-docdat.
    wa_saida_0100-netwr                 =  tg_zsdt0001-netwr.
    wa_saida_0100-placa_cav             =  tg_zsdt0001-placa_cav.
    wa_saida_0100-placa_car1            =  tg_zsdt0001-placa_car1.
    wa_saida_0100-placa_car2            =  tg_zsdt0001-placa_car2.
    wa_saida_0100-placa_car3            =  tg_zsdt0001-placa_car3.
    wa_saida_0100-motorista             =  tg_zsdt0001-motorista.
    wa_saida_0100-nr_ticket             =  tg_zsdt0001-nr_ticket.
    wa_saida_0100-dt_fechamento         =  tg_zsdt0001-dt_fechamento.
    wa_saida_0100-hr_fechamento         =  tg_zsdt0001-hr_fechamento.
    wa_saida_0100-dt_abertura           =  tg_zsdt0001-dt_abertura.
    wa_saida_0100-hr_abertura           =  tg_zsdt0001-hr_abertura.
    wa_saida_0100-nr_perc_umidade       =  tg_zsdt0001-nr_perc_umidade.
    wa_saida_0100-nr_qtd_umidade        =  tg_zsdt0001-nr_qtd_umidade.
    wa_saida_0100-nr_perc_impureza      =  tg_zsdt0001-nr_perc_impureza.
    wa_saida_0100-nr_qtd_impureza       =  tg_zsdt0001-nr_qtd_impureza.
    wa_saida_0100-nr_perc_avaria        =  tg_zsdt0001-nr_perc_avaria.
    wa_saida_0100-nr_qtd_avaria         =  tg_zsdt0001-nr_qtd_avaria.
    wa_saida_0100-nr_perc_ardido        =  tg_zsdt0001-nr_perc_ardido.
    wa_saida_0100-nr_qtd_ardido         =  tg_zsdt0001-nr_qtd_ardido.
    wa_saida_0100-nr_perc_quebra        =  tg_zsdt0001-nr_perc_quebra.
    wa_saida_0100-nr_qtd_quebra         =  tg_zsdt0001-nr_qtd_quebra.
    wa_saida_0100-nr_perc_esverd        =  tg_zsdt0001-nr_perc_esverd.
    wa_saida_0100-nr_qtd_esverd         =  tg_zsdt0001-nr_qtd_esverd.
    wa_saida_0100-doc_rem               =  tg_zsdt0001-doc_rem.
    wa_saida_0100-tknum                 =  tg_zsdt0001-tknum.
    wa_saida_0100-doc_material          =  tg_zsdt0001-doc_material.
    wa_saida_0100-ano_material          =  tg_zsdt0001-ano_material.
    wa_saida_0100-peso_subtotal         =  tg_zsdt0001-peso_subtotal.
    wa_saida_0100-dt_chegada            =  tg_zsdt0001-dt_chegada.
    wa_saida_0100-id_referencia         =  tg_zsdt0001-id_referencia.
    wa_saida_0100-tp_transgenia         =  tg_zsdt0001-tp_transgenia.
    wa_saida_0100-ds_obs                =  tg_zsdt0001-ds_obs.
    wa_saida_0100-local_descarga        =  tg_zsdt0001-local_descarga.
    wa_saida_0100-tipo_entrada          =  tg_zsdt0001-tipo_entrada.
    IF p_chv_nf IS INITIAL.
      wa_saida_0100-ic_xml              = icon_led_yellow.
    ENDIF.

    IF wa_saida_0100-tipo_entrada IS NOT INITIAL.
      READ TABLE tg_zlest0129 WITH KEY tipo_entrada = wa_saida_0100-tipo_entrada BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida_0100-desc_operacao =  tg_zlest0129-desc_operacao.
      ENDIF.
    ENDIF.

    wa_saida_0100-peso_retido_est       =  tg_zsdt0001-peso_retido_est.
    wa_saida_0100-peso_liqret_est       =  tg_zsdt0001-peso_liqret_est.
    wa_saida_0100-peso_retido_real      =  tg_zsdt0001-peso_retido_real.
    wa_saida_0100-peso_liqret_real      =  tg_zsdt0001-peso_liqret_real.
    wa_saida_0100-q_s                   = tg_zsdt0001-peso_subtotal - tg_zsdt0001-peso_fiscal. "Quebra/Sobra
    wa_saida_0100-chave_nfe             = tg_zsdt0001-chave_nfe.
    wa_saida_0100-modal                 = tg_zsdt0001-modal.
    wa_saida_0100-peso_rateio_origem    = tg_zsdt0001-peso_rateio_origem.

    IF ( p_chv_nf IS NOT INITIAL ) AND ( wa_saida_0100-chave_nfe IS INITIAL ).
      IF wa_saida_0100-doc_rem IS NOT INITIAL. "Próprio

        "Busca da Chave
        LOOP AT tg_vbfa WHERE vbelv = wa_saida_0100-doc_rem.
          READ TABLE tg_j_1bnflin WITH KEY refkey = tg_vbfa-vbeln_refkey BINARY SEARCH.

          CHECK sy-subrc = 0.

          READ TABLE tg_j_1bnfe_active WITH KEY docnum = tg_j_1bnflin-docnum BINARY SEARCH.

          IF ( sy-subrc EQ 0 ) AND ( tg_j_1bnfe_active-nfnum9 = tg_zsdt0001-nfnum ).
            "Concatenar a chave da nota fiscal
            CONCATENATE tg_j_1bnfe_active-regio
                        tg_j_1bnfe_active-nfyear
                        tg_j_1bnfe_active-nfmonth
                        tg_j_1bnfe_active-stcd1
                        tg_j_1bnfe_active-model
                        tg_j_1bnfe_active-serie
                        tg_j_1bnfe_active-nfnum9
                        tg_j_1bnfe_active-docnum9
                        tg_j_1bnfe_active-cdv
                   INTO wa_saida_0100-chave_nfe.

            wa_saida_0100-ic_xml  =  icon_led_green.
          ENDIF.
        ENDLOOP.

      ELSE. "Terceiro

        READ TABLE tg_zib_nfe_dist_ter WITH KEY forne_cnpj = tg_zsdt0001-forne_cnpj
                                                numero     = tg_zsdt0001-numero
                                                serie      = tg_zsdt0001-serie BINARY SEARCH.
        IF ( sy-subrc EQ 0 ).
          wa_saida_0100-chave_nfe = tg_zib_nfe_dist_ter-chave_nfe.
          wa_saida_0100-ic_xml    = icon_led_green.
        ENDIF.

      ENDIF.
    ENDIF.

    APPEND wa_saida_0100 TO it_saida_0100.

  ENDLOOP.

  "" AHSS - Chamado 140376 - 18/06/2024 - Ajuste para filtrar pela chave ##Inicio
  IF p_knfe IS NOT INITIAL.

    DELETE it_saida_0100 WHERE chave_nfe NOT IN p_knfe.

  ENDIF.
  "" AHSS - Chamado 140376 - 18/06/2024 - Ajuste para filtrar pela chave ##Fim

ENDFORM.

FORM f_imprimir_dados_classif.

  PERFORM f_definir_eventos.
  PERFORM f_montar_layout_classif.

  gs_layout-box_fieldname = 'SEL'.
  gs_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = v_report
      is_variant               = gs_variant_c
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'PF_STATUS_SET'
      it_fieldcat              = it_estrutura[]
      is_layout                = gs_layout
      i_save                   = 'X'
      it_events                = events
      is_print                 = t_print
    TABLES
      t_outtab                 = it_saida_0200.

ENDFORM.

FORM f_imprimir_dados.

  PERFORM f_definir_eventos.
  PERFORM f_montar_layout.

  gs_layout-box_fieldname = 'SEL'.
  gs_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = v_report
      is_variant               = gs_variant_c
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'PF_STATUS_SET'
      it_fieldcat              = it_estrutura[]
      is_layout                = gs_layout
      i_save                   = 'X'
      it_events                = events
      is_print                 = t_print
    TABLES
      t_outtab                 = it_saida_0100.

ENDFORM.

FORM f_definir_eventos .
  PERFORM f_carregar_eventos USING: slis_ev_top_of_page   'XTOP_OF_PAGE',
                                    slis_ev_pf_status_set 'PF_STATUS_SET'.

ENDFORM.                    " DEFINIR_EVENTOS

FORM f_carregar_eventos USING name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " F_CARREGAR_EVENTOS

FORM f_montar_estrutura USING VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_hotspot)
                              VALUE(p_just).

  CLEAR wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-hotspot       = p_hotspot.
  wa_estrutura-just          = p_just.
  wa_estrutura-ddictxt       = 'L'.
  wa_estrutura-outputlen     = p_outputlen.


  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO it_estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM f_montar_layout_classif.

  REFRESH:  it_estrutura[].

  PERFORM f_montar_estrutura USING:

    01  ''  ''                  'IT_SAIDA_0200' 'ICON_REENVIO'        'Sts.Reenvio SAP'          '16' ''    'C',
    02  ''  ''                  'IT_SAIDA_0200' 'NR_ROMANEIO'         'Romaneio'                 '10' ''    '',
    03  ''  ''                  'IT_SAIDA_0200' 'VBELN'               'OV/PED'                   '10' ''    '',
    04  ''  ''                  'IT_SAIDA_0200' 'DT_MOVIMENTO'        'Dt.Movimento'             '12' ''    '',
    05  ''  ''                  'IT_SAIDA_0200' 'NR_SAFRA'            'Safra'                    '06' ''    '',
    06  ''  ''                  'IT_SAIDA_0200' 'BUKRS'               'Empresa'                  '07' ''    '',
    07  ''  ''                  'IT_SAIDA_0200' 'BRANCH'              'Filial'                   '06' ''    '',
    08  'J_1BNFNAD'  'PARID'    'IT_SAIDA_0200' 'PARID'               'Origem'                   '10' ''    '',
    09  ''  ''                  'IT_SAIDA_0200' 'ID_CLI_DEST'         'Destino'                  '15' ''    '',
    10  'MAKT'  'MATNR'         'IT_SAIDA_0200' 'MATNR'               'Material'                 '10' ''    '',
    11  ''  ''                  'IT_SAIDA_0200' 'PESO_LIQ'            'Peso Líquido'             '13' ''    '',
    12  ''  ''                  'IT_SAIDA_0200' 'PESO_FISCAL'         'Peso Fiscal'              '13' ''    '',
    13  ''  ''                  'IT_SAIDA_0200' 'PLACA_CAV'           'Placa Cavalo'             '11' ''    '',
    14  ''  ''                  'IT_SAIDA_0200' 'MOTORISTA'           'Motorista'                '09' ''    '',
*
    15  ''  ''                  'IT_SAIDA_0200' 'NR_PERC_UMIDADE_S'   'Perc.Umidade S'           '20' ''    '',
    16  ''  ''                  'IT_SAIDA_0200' 'NR_QTD_UMIDADE_S'    'Qtd.Umidade S'            '20' ''    '',
    17  ''  ''                  'IT_SAIDA_0200' 'NR_PERC_IMPUREZA_S'  'Perc.Impureza S'          '20' ''    '',
    18  ''  ''                  'IT_SAIDA_0200' 'NR_QTD_IMPUREZA_S'   'Qtd.Impureza S'           '20' ''    '',
    19  ''  ''                  'IT_SAIDA_0200' 'NR_PERC_AVARIA_S'    'Perc.Avaria S'            '20' ''    '',
    20  ''  ''                  'IT_SAIDA_0200' 'NR_QTD_AVARIA_S'     'Qtd.Avaria S'             '20' ''    '',
    21  ''  ''                  'IT_SAIDA_0200' 'NR_PERC_ARDIDO_S'    'Perc.Ardido S'            '20' ''    '',
    22  ''  ''                  'IT_SAIDA_0200' 'NR_QTD_ARDIDO_S'     'Qtd.Ardido S'             '20' ''    '',
    23  ''  ''                  'IT_SAIDA_0200' 'NR_PERC_QUEBRA_S'    'Perc.Quebra S'            '20' ''    '',
    24  ''  ''                  'IT_SAIDA_0200' 'NR_QTD_QUEBRA_S'     'Qtd.Quebra S'             '20' ''    '',
    25  ''  ''                  'IT_SAIDA_0200' 'NR_PERC_ESVERD_S'    'Perc.Esverdeado S'        '20' ''    '',
    26  ''  ''                  'IT_SAIDA_2100' 'NR_QTD_ESVERD_S'     'Qtd.Esverdeado S'         '20' ''    '',
*
    27  ''  ''                  'IT_SAIDA_0200' 'DOC_REM'             'Remessa'                  '10' 'X'   '',
    28  ''  ''                  'IT_SAIDA_0200' 'TP_TRANSGENIA'       'Transgenia'               '10' ''    'C',
    29  ''  ''                  'IT_SAIDA_0200' 'FATURA_PROD'         'Fatura Prod.'             '15' ''    '',
    30  ''  ''                  'IT_SAIDA_0200' 'NRO_NF_PROD'         'Nf.Prod.'                 '15' ''    '',
    31  ''  ''                  'IT_SAIDA_0200' 'DOC_TRANSP'          'Doc.Transp.'              '15' ''    '',
    32  ''  ''                  'IT_SAIDA_0200' 'FKNUM'               'Doc.Custo'                '15' ''    '',
    33  ''  ''                  'IT_SAIDA_0200' 'OV_FRETE'            'Ov.Frete'                 '15' ''    '',
    34  ''  ''                  'IT_SAIDA_0200' 'FATURA_FRETE'        'Fat.frete'                '15' ''    '',
    35  ''  ''                  'IT_SAIDA_0200' 'NRO_NF_FRETE'        'Nf.Frete'                 '15' ''    '',
    36  ''  ''                  'IT_SAIDA_0200' 'AGENTE_FRETE'        'Agente Frete'             '25' ''    '',
    37  ''  ''                  'IT_SAIDA_0200' 'KBETR'               'Preço Frete'              '15' ''    '',
    38  ''  ''                  'IT_SAIDA_0200' 'PESO_LIQ_COMERCIAL'  'Peso Liq.Comercial'       '20' ''    '',
*
    39  ''  ''                  'IT_SAIDA_0200' 'NR_PERC_UMIDADE_E'   'Perc.Umidade E'           '20' ''    '',
    40  ''  ''                  'IT_SAIDA_0200' 'NR_QTD_UMIDADE_E'    'Qtd.Umidade E'            '20' ''    '',
    41  ''  ''                  'IT_SAIDA_0200' 'NR_PERC_IMPUREZA_E'  'Perc.Impureza E'          '20' ''    '',
    42  ''  ''                  'IT_SAIDA_0200' 'NR_QTD_IMPUREZA_E'   'Qtd.Impureza E'           '20' ''    '',
    43  ''  ''                  'IT_SAIDA_0200' 'NR_PERC_AVARIA_E'    'Perc.Avaria E'            '20' ''    '',
    44  ''  ''                  'IT_SAIDA_0200' 'NR_QTD_AVARIA_E'     'Qtd.Avaria E'             '20' ''    '',
    45  ''  ''                  'IT_SAIDA_0200' 'NR_PERC_ARDIDO_E'    'Perc.Ardido E'            '20' ''    '',
    46  ''  ''                  'IT_SAIDA_0200' 'NR_QTD_ARDIDO_E'     'Qtd.Ardido E'             '20' ''    '',
    47  ''  ''                  'IT_SAIDA_0200' 'NR_PERC_QUEBRA_E'    'Perc.Quebra E'            '20' ''    '',
    48  ''  ''                  'IT_SAIDA_0200' 'NR_QTD_QUEBRA_E'     'Qtd.Quebra E'             '20' ''    '',
    49  ''  ''                  'IT_SAIDA_0200' 'NR_PERC_ESVERD_E'    'Perc.Esverdeado E'        '20' ''    '',
    50  ''  ''                  'IT_SAIDA_0200' 'NR_QTD_ESVERD_E'     'Qtd.Esverdeado E'         '20' ''    '',
    51  ''  ''                  'IT_SAIDA_0200' 'DATACHEGADA'         'Dt.Chegada'               '15' ''    '',
    52  ''  ''                  'IT_SAIDA_0200' 'PESOCHEGADA'         'Peso Chegada'             '20' ''    '',
    53  ''  ''                  'IT_SAIDA_0200' 'ICON_EUDR'           'Atende EUDR'              '12' ''    'C'.

ENDFORM.                    " MONTAR_LAYOUT

FORM f_montar_layout .

  REFRESH:  it_estrutura[].

  PERFORM f_montar_estrutura USING:

    01  ''  ''                  'IT_SAIDA_0100' 'ICON_REENVIO'        'Sts.Reenvio SAP'          '16' ''    'C',
    57  ''  ''                  'IT_SAIDA_0100' 'ICON_EUDR'           'Atende EUDR'              '12' ''    'C',
    01  ''  ''                  'IT_SAIDA_0100' 'CH_REFERENCIA'       'Chv.Ref.'                 '20' ''    '',
    01  ''  ''                  'IT_SAIDA_0100' 'IC_MOV'              'Mov.'                     '04' ''    'C',
    01  ''  ''                  'IT_SAIDA_0100' 'TP_MOVIMENTO'        'Tp.Mov'                   '06' ''    'C',
    02  ''  ''                  'IT_SAIDA_0100' 'NR_ROMANEIO'         'Romaneio'                 '10' ''    '',
    03  ''  ''                  'IT_SAIDA_0100' 'VBELN'               'OV.'                      '10' ''    '',
    04  ''  ''                  'IT_SAIDA_0100' 'DT_MOVIMENTO'        'Dt.Movimento'             '12' ''    '',
    05  ''  ''                  'IT_SAIDA_0100' 'NR_SAFRA'            'Safra'                    '06' ''    '',
    06  ''  ''                  'IT_SAIDA_0100' 'BUKRS'               'Empresa'                  '07' ''    '',
    07  ''  ''                  'IT_SAIDA_0100' 'BRANCH'              'Centro'                   '06' ''    '',
    08  'J_1BNFNAD'  'PARID'    'IT_SAIDA_0100' 'PARID'               'Parceiro'                 '10' ''    '',
    08  ''  ''                  'IT_SAIDA_0100' 'NAME1'               'Nome Parceiro'            '10' ''    '',
    08  ''  ''                  'IT_SAIDA_0100' 'STCD1'               'CNPJ'                     ''   ''    '',
    09  ''  ''                  'IT_SAIDA_0100' 'TP_FRETE'            'Tp.Frete'                 '08' ''    'C',
    10  'MAKT'  'MATNR'         'IT_SAIDA_0100' 'MATNR'               'Material'                 '10' ''    '',
    10  'MAKT'  'MAKTX'         'IT_SAIDA_0100' 'MAKTX'               'Desc.Material'            ''   ''    '',
    11  ''  ''                  'IT_SAIDA_0100' 'PESO_LIQ'            'Peso Líquido'             '13' ''    '',
    12  ''  ''                  'IT_SAIDA_0100' 'PESO_FISCAL'         'Peso Origem'              '13' ''    '',
    13  ''  ''                  'IT_SAIDA_0100' 'NFNUM'               'Nota Fiscal'              '11' ''    '',
    14  ''  ''                  'IT_SAIDA_0100' 'SERIES'              'Série'                    '05' ''    '',
    15  ''  ''                  'IT_SAIDA_0100' 'DOCDAT'              'Data.Doc'                 '12' ''    '',
    16  ''  ''                  'IT_SAIDA_0100' 'NETWR'               'Valor NF'                 '13' ''    '',
    17  ''  ''                  'IT_SAIDA_0100' 'CHAVE_NFE'           'Chave NFe'                '44' ''    '',
    17  ''  ''                  'IT_SAIDA_0100' 'IC_XML'              'XML'                      '04' ''    'C',
    17  ''  ''                  'IT_SAIDA_0100' 'PLACA_CAV'           'Placa Cav.'               '11' ''    '',
    18  ''  ''                  'IT_SAIDA_0100' 'PLACA_CAR1'          'Placa Car.1'              '11' ''    '',
    19  ''  ''                  'IT_SAIDA_0100' 'PLACA_CAR2'          'Placa Car.2'              '11' ''    '',
    20  ''  ''                  'IT_SAIDA_0100' 'PLACA_CAR3'          'Placa Car.3'              '11' ''    '',
    21  ''  ''                  'IT_SAIDA_0100' 'MOTORISTA'           'Motorista'                '09' ''    '',
    22  ''  ''                  'IT_SAIDA_0100' 'NR_TICKET'           'Nr.Ticket'                '10' ''    '',
    23  ''  ''                  'IT_SAIDA_0100' 'DT_ABERTURA'         'Dt.Abertura'              '13' ''    '',
    23  ''  ''                  'IT_SAIDA_0100' 'HR_ABERTURA'         'Hr.Abertura'              '13' ''    '',
    24  ''  ''                  'IT_SAIDA_0100' 'DT_FECHAMENTO'       'Dt.Fechamento'            '13' ''    '',
    24  ''  ''                  'IT_SAIDA_0100' 'HR_FECHAMENTO'       'Hr.Fechamento'            '13' ''    '',
    25  ''  ''                  'IT_SAIDA_0100' 'NR_PERC_UMIDADE'     'Perc.Umid.'               '12' ''    '',
    26  ''  ''                  'IT_SAIDA_0100' 'NR_QTD_UMIDADE'      'Umidade'                  '12' ''    '',
    27  ''  ''                  'IT_SAIDA_0100' 'NR_PERC_IMPUREZA'    'Perc.Imp.'                '12' ''    '',
    28  ''  ''                  'IT_SAIDA_0100' 'NR_QTD_IMPUREZA'     'Impureza'                 '12' ''    '',
    29  ''  ''                  'IT_SAIDA_0100' 'NR_PERC_AVARIA'      'Perc.Avaria.'             '12' ''    '',
    30  ''  ''                  'IT_SAIDA_0100' 'NR_QTD_AVARIA'       'Avariado'                 '12' ''    '',
    31  ''  ''                  'IT_SAIDA_0100' 'NR_PERC_ARDIDO'      'Perc.Ardido'              '12' ''    '',
    32  ''  ''                  'IT_SAIDA_0100' 'NR_QTD_ARDIDO'       'Ardido'                   '12' ''    '',
    33  ''  ''                  'IT_SAIDA_0100' 'NR_PERC_QUEBRA'      'Perc.Quebra'              '12' ''    '',
    34  ''  ''                  'IT_SAIDA_0100' 'NR_QTD_QUEBRA'       'Quebra'                   '12' ''    '',
    35  ''  ''                  'IT_SAIDA_0100' 'NR_PERC_ESVERD'      'Perc.Esverd.'             '12' ''    '',
    36  ''  ''                  'IT_SAIDA_0100' 'NR_QTD_ESVERD'       'Esverdeado'               '12' ''    '',
    37  ''  ''                  'IT_SAIDA_0100' 'DOC_REM'             'Remessa'                  '10' 'X'   '',
    38  ''  ''                  'IT_SAIDA_0100' 'TKNUM'               'Transporte'               '10' ''    '',
    39  ''  ''                  'IT_SAIDA_0100' 'DOC_MATERIAL'        'Doc.Material'             '12' ''    '',
    40  ''  ''                  'IT_SAIDA_0100' 'ANO_MATERIAL'        'Ano.Material'             '12' ''    '',
    41  ''  ''                  'IT_SAIDA_0100' 'PESO_SUBTOTAL'       'Peso.Subtotal'            '13' ''    '',
    42  ''  ''                  'IT_SAIDA_0100' 'DT_CHEGADA'          'Dt.Chegada'               '13' ''    '',
    43  ''  ''                  'IT_SAIDA_0100' 'ID_REFERENCIA'       'Id.Ref.'                  '10' ''    '',
    44  ''  ''                  'IT_SAIDA_0100' 'TP_TRANSGENIA'       'Transgenia'               '10' ''    'C',
    45  ''  ''                  'IT_SAIDA_0100' 'DS_OBS'              'Observação'               '15' ''    '',
    46  ''  ''                  'IT_SAIDA_0100' 'LOCAL_DESCARGA'      'Loc.Descarga'             '12' ''    'C',
    47  ''  ''                  'IT_SAIDA_0100' 'TIPO_ENTRADA'        'Tp.Entrada'               '10' ''    'C',
    47  ''  ''                  'IT_SAIDA_0100' 'DESC_OPERACAO'       'Operação'                 '13' ''    '',
    48  ''  ''                  'IT_SAIDA_0100' 'PESO_RETIDO_EST'     'Retenção Prev.'           '14' ''    '',
    49  ''  ''                  'IT_SAIDA_0100' 'PESO_LIQRET_EST'     'Líq.A.R.Prev.'            '14' ''    '',
    50  ''  ''                  'IT_SAIDA_0100' 'PESO_RETIDO_REAL'    'Retenção Real.'           '14' ''    '',
    51  ''  ''                  'IT_SAIDA_0100' 'PESO_LIQRET_REAL'    'Líq.A.R.Real.'            '14' ''    '',
    52  ''  ''                  'IT_SAIDA_0100' 'Q_S'                 'Q/S'                      '13' ''    '',
    53  'ZSDT0001'  'ST_CCT'    'IT_SAIDA_0100' 'ST_CCT'              'St.CCT'                   '06' ''    '',
    54  ''  ''                  'IT_SAIDA_0100' 'MODAL'               'Modal'                    '06' ''    '',
    55  ''  ''                  'IT_SAIDA_0100' 'PESO_RATEIO_ORIGEM'  'Peso Rateio Origem'       '13' ''    '',
    56  ''  ''                  'IT_SAIDA_0100' 'CFOP'                'CFOP'                     '04' ''    ''.

ENDFORM.                    " MONTAR_LAYOUT

FORM user_command  USING r_ucomm      LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.

  DATA: ref1           TYPE REF TO cl_gui_alv_grid,
        wl_zob_ret_msg TYPE zob_ret_msg.

  CASE r_ucomm.
    WHEN: '&IC1'.

      IF ( rs_selfield-fieldname EQ 'DOC_REM' ).
        IF p_romane = abap_true.
          CLEAR: wa_saida_0100.
          READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX rs_selfield-tabindex.
          CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100-doc_rem IS NOT INITIAL ).
          SET PARAMETER ID 'VL'  FIELD wa_saida_0100-doc_rem.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
        ELSE.
          CLEAR: wa_saida_0200.
          READ TABLE it_saida_0200 INTO wa_saida_0200 INDEX rs_selfield-tabindex.
          CHECK ( sy-subrc = 0 ) AND ( wa_saida_0200-doc_rem IS NOT INITIAL ).
          SET PARAMETER ID 'VL'  FIELD wa_saida_0200-doc_rem.
          CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
  ENDCASE.

  IF ref1 IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR' "<-- Gets the reference of ALV grid object from REUSE functions
      IMPORTING
        e_grid = ref1.
  ENDIF.

  CHECK ref1 IS NOT INITIAL.

  CASE r_ucomm.
    WHEN 'ATUALIZAR'.
      IF p_romane = abap_true.
        PERFORM f_selecionar_dados.
        PERFORM f_processar_dados.
      ELSE.
        PERFORM f_selecionar_dados_classif.
        PERFORM f_processar_dados_classif.
      ENDIF.

    WHEN 'REENVIO_RO'.
      CALL METHOD ref1->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows.

      IF it_sel_rows[] IS INITIAL.
        MESSAGE 'Nenhuma linha selecionada!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF p_romane = abap_true.
        LOOP AT it_sel_rows INTO wa_sel_rows.
          CLEAR: wl_zob_ret_msg.
          READ TABLE it_saida_0100 ASSIGNING FIELD-SYMBOL(<fs_out>) INDEX wa_sel_rows-index.
          CHECK sy-subrc = 0.

          wl_zob_ret_msg-msg_v1         = <fs_out>-ch_referencia.
          wl_zob_ret_msg-msg_v2         = <fs_out>-nr_romaneio.
          wl_zob_ret_msg-cd_processo    = '001'.

          CALL FUNCTION 'Z_SOLIC_REENVIO_MSG_SAP'
            EXPORTING
              i_zob_ret_msg = wl_zob_ret_msg.

          IF sy-subrc NE 0.
            RETURN.
          ENDIF.

          <fs_out>-icon_reenvio = icon_yellow_light.
        ENDLOOP.
      ELSE.
        LOOP AT it_sel_rows INTO wa_sel_rows.
          CLEAR: wl_zob_ret_msg.
          READ TABLE it_saida_0200 ASSIGNING FIELD-SYMBOL(<fs_out2>) INDEX wa_sel_rows-index.
          CHECK sy-subrc = 0.

          wl_zob_ret_msg-msg_v1         = <fs_out2>-ch_referencia.
          wl_zob_ret_msg-msg_v2         = <fs_out2>-nr_romaneio.
          wl_zob_ret_msg-cd_processo    = '001'.

          CALL FUNCTION 'Z_SOLIC_REENVIO_MSG_SAP'
            EXPORTING
              i_zob_ret_msg = wl_zob_ret_msg.

          IF sy-subrc NE 0.
            RETURN.
          ENDIF.

          <fs_out2>-icon_reenvio = icon_yellow_light.
        ENDLOOP.
      ENDIF.

      MESSAGE 'Solicitação(ões) de reenvio processada(s) com êxito!' TYPE 'S'.

  ENDCASE.

  CALL METHOD ref1->refresh_table_display.



ENDFORM.                    "user_command

FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.

ENDFORM. "X_TOP_PAGE

FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO

FORM pf_status_set USING ut_extab TYPE slis_t_extab.        "#EC CALLED

  "DELETE UT_EXTAB WHERE FCODE = '&REFRESH'.

  SET PF-STATUS '0100' EXCLUDING ut_extab.

  "SET PF-STATUS 'STANDARD_FULLSCREEN' OF PROGRAM 'SAPLKKBL'.


ENDFORM.
