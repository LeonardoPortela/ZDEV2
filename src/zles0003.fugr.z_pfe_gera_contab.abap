FUNCTION z_pfe_gera_contab.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_NM_LOTE) TYPE  ZPFE_NUMERO_LOTE
*"     REFERENCE(P_NM_LOTE_ITEM) TYPE  ZPFE_NUMERO_LOTE
*"  EXPORTING
*"     VALUE(WA_LOTE_ITEM) TYPE  ZPFE_LOTE_ITEM
*"     VALUE(WA_LOTE_ITEM_ALV) TYPE  ZPFE_LOTE_ITEM_ALV
*"  TABLES
*"      IT_LOTE_ITEM STRUCTURE  ZPFE_LOTE_ITEM
*"  EXCEPTIONS
*"      SEM_LOTE
*"      SEM_LOTE_ITEM
*"      GERANDO_CONTB
*"      CONCLUIDO_CONTB
*"      GERADO_CONTB
*"      CONTA_DEB_CRED
*"      SEM_PROPRIETARIO
*"      NAO_CONFERIDO
*"      ERROR
*"      SEM_DATA_BAIXA
*"----------------------------------------------------------------------

  DATA: l_monat          TYPE monat,
        l_gjahr          TYPE gjahr,
        p_data_ent       TYPE datum,
        p_data_val       TYPE datum,
        l_exige_ccusto   TYPE c LENGTH 1,
        l_exige_material TYPE c LENGTH 1,
        p_codtrp         TYPE c LENGTH 10,
        lc_centro        TYPE werks_d,
        wa_lips          TYPE lips,
        wa_j_1bnfdoc     TYPE j_1bnfdoc,
        wa_j_1bnflin     TYPE j_1bnflin,
        wa_j_1bbranch    TYPE j_1bbranch.

  DATA: p_valor            TYPE netwr_fp,
        p_cd_lote_finan    TYPE char10,
        wa_lote            TYPE zpfe_lote,
        wa_lote_alv        TYPE zpfe_lote_alv,
        it_lotes_item      TYPE TABLE OF zpfe_lote_item,
        it_lotes_item_alv  TYPE TABLE OF zpfe_lote_item_alv,
        pnrlote            TYPE lxhme_range_c10_t WITH HEADER LINE,
        wa_zlest0025       TYPE zlest0025,
        wa_zlest0018       TYPE zlest0018,
        wa_zcte_trans      TYPE zcte_trans,
        wa_setleaf         TYPE setleaf,
        wa_msg_lote        TYPE zpfe_lote_msg,
        it_setleaf         LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE,
        it_zib_contabil    TYPE TABLE OF zib_contabil WITH HEADER LINE,
        vg_dias            TYPE c LENGTH 2,
        lc_fabkl           TYPE fabkl,
        lp_fkday           TYPE fkday,
        it_zpfe_chvid_ag   TYPE TABLE OF zpfe_chvid_ag WITH HEADER LINE,
        wa_lote_item_ag    TYPE zpfe_lote_item,
        it_lote_item_ag    TYPE TABLE OF zpfe_lote_item WITH HEADER LINE,
        wa_zcte_identifica TYPE zcte_identifica,
        vg_valor_ajuste    TYPE kwert,
        vg_chave_branco    TYPE c LENGTH 1.


  DATA: it_lote_item_msg TYPE TABLE OF zpfe_lote_item WITH HEADER LINE.

  " Variáveis de BAPI_ACC_DOCUMENT_POST
  DATA: vbudat            TYPE sy-datum,
        vbldat            TYPE sy-datum,
        wa_returnobj      LIKE zfie_returnobj,
        gd_documentheader LIKE bapiache09,
        wa_payable        LIKE bapiacap09,
        wa_currencyamount LIKE bapiaccr09,
        wa_accountgl      LIKE bapiacgl09,
        wa_lfa1           LIKE lfa1,
        wa_bapiret        LIKE bapiret2,
        vg_datum          LIKE sy-datum,
        vg_uzeit          LIKE sy-uzeit,
        vg_belnr          LIKE zpfe_lote_item-belnr,
        wa_ret_document   TYPE zfie_ret_document,
        it_ret_document   LIKE STANDARD TABLE OF wa_ret_document,
        wa_return	        LIKE zfie_return,
        seqimsg           TYPE ze_seqlan,
        vg_awtyp          LIKE bkpf-awtyp,
        vg_awkey          LIKE bkpf-awkey,
        vg_awsys          LIKE bkpf-awsys,
        vg_bukrs          LIKE bkpf-bukrs,
        vg_gjahr          LIKE bkpf-gjahr,
        it_return         TYPE TABLE OF zfie_return WITH HEADER LINE,
        it_payable        LIKE STANDARD TABLE OF wa_payable,
        it_accountgl      LIKE STANDARD TABLE OF wa_accountgl,
        it_currencyamount LIKE STANDARD TABLE OF wa_currencyamount,
        it_criteria       LIKE bapiackec9 OCCURS 0 WITH HEADER LINE,
        it_bapiret        LIKE STANDARD TABLE OF wa_bapiret,
        it_erro_contabil  LIKE STANDARD TABLE OF wa_ret_document,
        vg_posnr_acc      TYPE posnr_acc,
        var_tomador       TYPE c,
        var_parid         TYPE j_1bbranch-branch,

        BEGIN OF it_tipo_conta OCCURS 0,
          seqitem   TYPE zib_contabil-seqitem,
          tipo      TYPE zlest0018-tipoconta_d,
          tipoch(1),
        END OF it_tipo_conta.

  pnrlote-sign    = 'I'.
  pnrlote-option  = 'EQ'.
  pnrlote-low     = p_nm_lote.
  pnrlote-high    = p_nm_lote.
  APPEND pnrlote.

  SELECT SINGLE *
    INTO wa_lote_item
    FROM zpfe_lote_item
   WHERE nm_lote      EQ p_nm_lote
     AND nm_lote_item EQ p_nm_lote_item.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e017 WITH p_nm_lote p_nm_lote_item RAISING sem_lote_item.
  ENDIF.

  IF ( wa_lote_item-dt_baixa IS INITIAL  ) AND ( sy-ucomm EQ 'GERA_CONTB' ).
    MESSAGE e031 WITH p_nm_lote_item RAISING sem_data_baixa.
  ENDIF.

  SELECT SINGLE * INTO wa_j_1bnflin
    FROM j_1bnflin
   WHERE docnum EQ wa_lote_item-docnum.

  SELECT SINGLE * INTO wa_zcte_identifica
    FROM zcte_identifica
   WHERE docnum EQ wa_lote_item-docnum.

  IF wa_zcte_identifica-dc_fornecimento IS NOT INITIAL.
    SELECT SINGLE * INTO wa_lips
      FROM lips
     WHERE vbeln EQ wa_zcte_identifica-dc_fornecimento.
  ENDIF.

  CLEAR: it_zpfe_chvid_ag[].

  SELECT SINGLE * FROM j_1bnfdoc
    INTO wa_j_1bnfdoc
    WHERE docnum EQ wa_lote_item-docnum.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = wa_j_1bnfdoc-parid
    IMPORTING
      output = var_parid.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = var_parid
    IMPORTING
      output = var_parid.

  SELECT SINGLE * FROM j_1bbranch
    INTO wa_j_1bbranch
  WHERE branch EQ var_parid.

  CASE sy-subrc.
    WHEN: 0.
      IF ( wa_j_1bnfdoc-bukrs EQ wa_j_1bbranch-bukrs ).
        var_tomador = 'P'.
      ELSE.
        var_tomador = 'I'.
      ENDIF.
    WHEN OTHERS.
      var_tomador = 'T'.
  ENDCASE.


  SELECT * INTO TABLE it_zpfe_chvid_ag
    FROM zpfe_chvid_ag
   WHERE chvid_ch_vinc EQ wa_lote_item-chvid.

  CHECK it_zpfe_chvid_ag[] IS INITIAL.

  CLEAR: it_zpfe_chvid_ag[].

  SELECT * INTO TABLE it_zpfe_chvid_ag
    FROM zpfe_chvid_ag
   WHERE chvid_ch EQ wa_lote_item-chvid.

  "Saldo - Abate Ajuste (Quebra Perda - Contabilização)
  vg_valor_ajuste = 0.
  IF wa_lote_item-chvid = '2'.
    LOOP AT it_zpfe_chvid_ag.
      LOOP AT it_lote_item INTO wa_lote_item_ag WHERE nm_lote EQ wa_lote_item-nm_lote
                                                  AND chvid   EQ it_zpfe_chvid_ag-chvid_ch_vinc
                                                  AND docnum  EQ wa_lote_item-docnum.
        IF ( wa_lote_item_ag-chvid = '30' ) OR ( wa_lote_item_ag-chvid = '31' ).
          vg_valor_ajuste = vg_valor_ajuste + wa_lote_item_ag-vl_conferido.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  SELECT SINGLE * INTO wa_lote FROM zpfe_lote WHERE nm_lote EQ p_nm_lote.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e013 WITH p_nm_lote RAISING sem_lote.
  ENDIF.

  SELECT SINGLE * INTO wa_zcte_trans
    FROM zcte_trans
   WHERE docnum     EQ wa_lote_item-docnum
     AND tp_veiculo EQ '0'.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e025 WITH wa_lote_item-docnum RAISING sem_proprietario.
  ENDIF.

  MOVE-CORRESPONDING wa_lote TO wa_lote_alv.

  IF wa_lote_item-ck_conferido IS INITIAL.
    MESSAGE e027 WITH p_nm_lote p_nm_lote_item RAISING nao_conferido.
  ENDIF.

  CASE wa_lote_item-tp_plano_administradora.
    WHEN zcl_ciot=>st_tp_plano_pre_pago.

      tip_contabil = 'FP'.
      "C  Confirmado
      IF wa_lote_item-status EQ 'C'.
        MESSAGE e019 WITH p_nm_lote p_nm_lote_item RAISING concluido_contb.
      ENDIF.

    WHEN OTHERS.

      CALL FUNCTION 'Z_PFE_TIPO_CONTAB'
        EXPORTING
          p_dt_posicao  = wa_lote-dt_posicao
        IMPORTING
          p_tipcontabil = tip_contabil.

      CASE wa_lote_item-status.
        WHEN c_g.
          "G  Gerando Contabil
          MESSAGE e018 WITH p_nm_lote p_nm_lote_item RAISING gerando_contb.
        WHEN c_c.
          "C  Confirmado
          MESSAGE e019 WITH p_nm_lote p_nm_lote_item RAISING concluido_contb.
      ENDCASE.

  ENDCASE.

  SELECT SINGLE * INTO wa_zlest0025
    FROM zlest0025
   WHERE chvid EQ wa_lote_item-chvid.

  SELECT SINGLE * INTO wa_zlest0018 FROM zlest0018
   WHERE chvid       EQ wa_lote_item-chvid
     AND tipcontabil EQ tip_contabil
     AND ctlglancto  EQ wa_zlest0025-ctlglancto
     AND bukrs       EQ wa_lote-bukrs
     AND tp_tomador  EQ var_tomador.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e020 WITH wa_lote_item-chvid tip_contabil RAISING conta_deb_cred.
  ENDIF.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZPFEFINAN'
    IMPORTING
      number      = p_cd_lote_finan.

  IF 1 = 1.
    CONCATENATE p_nm_lote p_cd_lote_finan INTO it_zib_contabil-obj_key.

    it_zib_contabil-seqitem = 1.
    it_zib_contabil-bschl   = wa_zlest0018-chvlancto_d.
    it_zib_contabil-bukrs   = wa_lote-bukrs.
    it_zib_contabil-umskz   = wa_zlest0018-razaoesp_d.

    p_data_ent = wa_lote-dt_posicao.
    it_zib_contabil-interface = '97'.
    it_zib_contabil-bktxt     = 'PF-e'.


    IF tip_contabil EQ 'FS' OR tip_contabil EQ 'FP'.

      CALL FUNCTION 'Z_RET_DATA_MES_ABERTO'
        EXPORTING
          p_data_ent  = wa_lote-dt_posicao
          p_bukrs     = wa_lote-bukrs
        IMPORTING
          p_data_val  = p_data_val
        EXCEPTIONS
          sem_periodo = 1
          OTHERS      = 2.

    ELSE.
      CALL FUNCTION 'Z_RET_DATA_MES_ABERTO'
        EXPORTING
          p_data_ent  = wa_lote-augdt
          p_bukrs     = wa_lote-bukrs
        IMPORTING
          p_data_val  = p_data_val
        EXCEPTIONS
          sem_periodo = 1
          OTHERS      = 2.

    ENDIF.
    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING error.
    ENDIF.

*    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
*      EXPORTING
*        input  = p_data_val
*      IMPORTING
*        output = it_zib_contabil-bldat.

    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = wa_lote_item-dt_baixa
      IMPORTING
        output = it_zib_contabil-bldat.

    it_zib_contabil-budat     = it_zib_contabil-bldat.
    it_zib_contabil-zfbdt     = it_zib_contabil-bldat.
    "it_zib_contabil-gjahr     = p_data_val(4).
    it_zib_contabil-gjahr     = wa_lote_item-dt_baixa(4).
    "it_zib_contabil-monat     = p_data_val+4(2).
    it_zib_contabil-monat     = wa_lote_item-dt_baixa+4(2).
    it_zib_contabil-blart     = 'SF'.

    CONCATENATE wa_lote_item-ctenum '-' wa_lote_item-cteserie INTO it_zib_contabil-xblnr.

    it_tipo_conta-seqitem = it_zib_contabil-seqitem.
    it_tipo_conta-tipo    = wa_zlest0018-tipoconta_d.
    it_tipo_conta-tipoch  = 'D'.
    APPEND it_tipo_conta.

    CASE wa_zlest0018-tipoconta_d.
      WHEN 'FA'.

        CASE wa_zlest0025-tp_atribuicao_d.
          WHEN '01'.
            CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
          WHEN '02'.
            CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
          WHEN OTHERS.
            CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
        ENDCASE.

        it_zib_contabil-hkont = wa_lote-cd_adiministra.
        it_zib_contabil-gsber = wa_lote-branch.
      WHEN 'FS'.

        CASE wa_zlest0025-tp_atribuicao_d.
          WHEN '01'.
            CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
          WHEN '02'.
            CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
          WHEN OTHERS.
            CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
        ENDCASE.

        it_zib_contabil-hkont = wa_zcte_trans-proprietario.
        it_zib_contabil-gsber = wa_lote-branch.
      WHEN 'RZ'.

        CASE wa_zlest0025-tp_atribuicao_d.
          WHEN '01'.
            CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
          WHEN '02'.
            CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
          WHEN OTHERS.
            CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
        ENDCASE.

        it_zib_contabil-hkont = wa_zlest0018-contadebito.
        IF ( var_tomador EQ 'I' ).
          lc_centro =  wa_lote-branch.
        ELSE.
          lc_centro = wa_lips-werks.
        ENDIF.

        CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
          EXPORTING
            centro               = lc_centro
          IMPORTING
            centro_out           = lc_centro
          EXCEPTIONS
            informar_centro      = 1
            nao_centro_r_virtual = 2
            informar_centro_out  = 3
            informar_centro_v    = 4
            OTHERS               = 5.
        it_zib_contabil-gsber   = lc_centro.
    ENDCASE.

    it_zib_contabil-bupla = it_zib_contabil-gsber.
    it_zib_contabil-wrbtr = abs( wa_lote_item-vl_pago_lote + vg_valor_ajuste ).
    it_zib_contabil-waers = wa_lote-moeda.

    CONCATENATE wa_zlest0025-deschvid
                'CTR' wa_lote_item-nucontrato
                'DACTE' wa_lote_item-ctenum
           INTO it_zib_contabil-sgtxt SEPARATED BY space.
    CONCATENATE it_zib_contabil-sgtxt '/' wa_lote_item-cteserie INTO it_zib_contabil-sgtxt.

    it_zib_contabil-waers_i       = wa_lote-moeda.
    it_zib_contabil-dmbtr         = abs( wa_lote_item-vl_pago_lote + vg_valor_ajuste ).
    it_zib_contabil-rg_atualizado = c_n.

    CLEAR: it_zib_contabil-kostl, l_exige_ccusto, l_exige_material.
    PERFORM check_contarazao_exige_ccusto USING it_zib_contabil-hkont 'C' it_zib_contabil-bschl
                                       CHANGING l_exige_ccusto l_exige_material.
    IF l_exige_ccusto = 'X'.
      p_codtrp = wa_lote-branch.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = p_codtrp
        IMPORTING
          output = p_codtrp.
      PERFORM obtem_centro_custo_contarazao USING p_codtrp 'C' it_zib_contabil-hkont
                                         CHANGING it_zib_contabil-kostl.
    ENDIF.
    IF l_exige_material = 'X'.
      it_zib_contabil-matnr = wa_j_1bnflin-matnr.
    ELSE.
      CLEAR: it_zib_contabil-matnr.
    ENDIF.
    APPEND it_zib_contabil.

*
    it_zib_contabil-seqitem = 2.
    it_zib_contabil-bschl   = wa_zlest0018-chvlancto_c.
    it_zib_contabil-umskz   = wa_zlest0018-razaoesp_c.

    it_tipo_conta-seqitem = it_zib_contabil-seqitem.
    it_tipo_conta-tipo    = wa_zlest0018-tipoconta_c.
    it_tipo_conta-tipoch  = 'C'.
    APPEND it_tipo_conta.


    "01	Nr. Lote
    "02	Nr. Contrato

    CASE wa_zlest0018-tipoconta_c.
      WHEN 'FA'.

        CASE wa_zlest0025-tp_atribuicao_c.
          WHEN '01'.
            CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
          WHEN '02'.
            CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
          WHEN OTHERS.
            CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
        ENDCASE.

        it_zib_contabil-hkont = wa_lote-cd_adiministra.
        it_zib_contabil-gsber = wa_lote-branch.
      WHEN 'FS'.

        CASE wa_zlest0025-tp_atribuicao_c.
          WHEN '01'.
            CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
          WHEN '02'.
            CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
          WHEN OTHERS.
            CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
        ENDCASE.

        it_zib_contabil-hkont = wa_zcte_trans-proprietario.
        it_zib_contabil-gsber = wa_lote-branch.
      WHEN 'RZ'.

        CASE wa_zlest0025-tp_atribuicao_c.
          WHEN '01'.
            CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
          WHEN '02'.
            CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
          WHEN OTHERS.
            CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
        ENDCASE.

        it_zib_contabil-hkont = wa_zlest0018-contacredito.
        IF ( var_tomador EQ 'I' ).
          lc_centro = wa_lote-branch.
        ELSE.
          lc_centro = wa_lips-werks.
        ENDIF.

        CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
          EXPORTING
            centro               = lc_centro
          IMPORTING
            centro_out           = lc_centro
          EXCEPTIONS
            informar_centro      = 1
            nao_centro_r_virtual = 2
            informar_centro_out  = 3
            informar_centro_v    = 4
            OTHERS               = 5.
        it_zib_contabil-gsber   = lc_centro.
    ENDCASE.

    it_zib_contabil-bupla = it_zib_contabil-gsber.

    CLEAR: it_zib_contabil-kostl, l_exige_ccusto, l_exige_material.
    PERFORM check_contarazao_exige_ccusto USING it_zib_contabil-hkont 'C' it_zib_contabil-bschl
                                       CHANGING l_exige_ccusto l_exige_material.
    IF l_exige_ccusto = 'X'.
      p_codtrp = wa_lote-branch.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = p_codtrp
        IMPORTING
          output = p_codtrp.
      PERFORM obtem_centro_custo_contarazao USING p_codtrp 'C' it_zib_contabil-hkont
                                         CHANGING it_zib_contabil-kostl.
    ENDIF.

    IF l_exige_material = 'X'.
      it_zib_contabil-matnr = wa_j_1bnflin-matnr.
    ELSE.
      CLEAR: it_zib_contabil-matnr.
    ENDIF.

    APPEND it_zib_contabil.


    wa_lote_item-status         = c_g.
    wa_lote_item-obj_key        = it_zib_contabil-obj_key.
    wa_lote_item-ds_usuario_ctb = sy-uname.
  ENDIF.

  "---------------------------------------------------------------------------------------------------
  "Chaves vinculados a chave do documento principal --------------------------------------------------
  "---------------------------------------------------------------------------------------------------
  CLEAR: it_lote_item_ag[].

  LOOP AT it_zpfe_chvid_ag.
    LOOP AT it_lote_item INTO wa_lote_item_ag WHERE nm_lote EQ wa_lote_item-nm_lote
                                                AND chvid   EQ it_zpfe_chvid_ag-chvid_ch_vinc
                                                AND docnum  EQ wa_lote_item-docnum.

      IF wa_lote_item_ag-vl_pago_lote EQ 0.
        CONCATENATE p_nm_lote p_cd_lote_finan INTO it_zib_contabil-obj_key.
        wa_lote_item_ag-status         = c_g.
        wa_lote_item_ag-obj_key        = it_zib_contabil-obj_key.
        wa_lote_item_ag-ds_usuario_ctb = sy-uname.
        APPEND wa_lote_item_ag TO it_lote_item_ag.
        IF wa_lote_item_ag-vl_diferenca EQ 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF wa_lote_item_ag-vl_pago_lote NE 0.

        SELECT SINGLE * INTO wa_zlest0025
          FROM zlest0025
         WHERE chvid EQ wa_lote_item_ag-chvid.

        SELECT SINGLE * INTO wa_zlest0018 FROM zlest0018
         WHERE chvid       EQ wa_lote_item_ag-chvid
           AND tipcontabil EQ tip_contabil
           AND ctlglancto  EQ wa_zlest0025-ctlglancto
           AND bukrs       EQ wa_lote-bukrs
           AND tp_tomador  EQ var_tomador.

        IF NOT sy-subrc IS INITIAL.
          MESSAGE e020 WITH wa_lote_item_ag-chvid tip_contabil RAISING conta_deb_cred.
        ENDIF.

        CONCATENATE p_nm_lote p_cd_lote_finan INTO it_zib_contabil-obj_key.

        it_zib_contabil-seqitem = it_zib_contabil-seqitem + 1.
        it_zib_contabil-bschl   = wa_zlest0018-chvlancto_d.
        it_zib_contabil-umskz   = wa_zlest0018-razaoesp_d.
        it_zib_contabil-wrbtr   = abs( wa_lote_item_ag-vl_pago_lote ).
        it_zib_contabil-dmbtr   = abs( wa_lote_item_ag-vl_pago_lote ).

        CONCATENATE wa_lote_item_ag-ctenum '-' wa_lote_item_ag-cteserie INTO it_zib_contabil-xblnr.

        it_tipo_conta-seqitem = it_zib_contabil-seqitem.
        it_tipo_conta-tipo    = wa_zlest0018-tipoconta_d.
        it_tipo_conta-tipoch  = 'D'.
        APPEND it_tipo_conta.

        CASE wa_zlest0018-tipoconta_d.
          WHEN 'FA'.

            CASE wa_zlest0025-tp_atribuicao_d.
              WHEN '01'.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
              WHEN '02'.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
              WHEN OTHERS.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
            ENDCASE.

            it_zib_contabil-hkont = wa_lote-cd_adiministra.
            it_zib_contabil-gsber   = wa_lote-branch.
          WHEN 'FS'.

            CASE wa_zlest0025-tp_atribuicao_d.
              WHEN '01'.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
              WHEN '02'.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
              WHEN OTHERS.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
            ENDCASE.

            it_zib_contabil-hkont = wa_zcte_trans-proprietario.
            it_zib_contabil-gsber   = wa_lote-branch.
          WHEN 'RZ'.

            CASE wa_zlest0025-tp_atribuicao_d.
              WHEN '01'.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
              WHEN '02'.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
              WHEN OTHERS.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
            ENDCASE.

            it_zib_contabil-hkont = wa_zlest0018-contadebito.

            IF ( var_tomador EQ 'I' ).
              lc_centro = wa_lote-branch.
            ELSE.
              lc_centro = wa_lips-werks.
            ENDIF.

            CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
              EXPORTING
                centro               = lc_centro
              IMPORTING
                centro_out           = lc_centro
              EXCEPTIONS
                informar_centro      = 1
                nao_centro_r_virtual = 2
                informar_centro_out  = 3
                informar_centro_v    = 4
                OTHERS               = 5.
            it_zib_contabil-gsber   = lc_centro.
        ENDCASE.

        it_zib_contabil-bupla = it_zib_contabil-gsber.

        CONCATENATE wa_zlest0025-deschvid
                    'CTR' wa_lote_item_ag-nucontrato
                    'DACTE' wa_lote_item_ag-ctenum
               INTO it_zib_contabil-sgtxt SEPARATED BY space.
        CONCATENATE it_zib_contabil-sgtxt '/' wa_lote_item_ag-cteserie INTO it_zib_contabil-sgtxt.

        CLEAR: it_zib_contabil-kostl, l_exige_ccusto, l_exige_material.
        PERFORM check_contarazao_exige_ccusto USING it_zib_contabil-hkont 'C' it_zib_contabil-bschl
                                           CHANGING l_exige_ccusto l_exige_material.
        IF l_exige_ccusto = 'X'.
          p_codtrp = wa_lote-branch.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = p_codtrp
            IMPORTING
              output = p_codtrp.
          PERFORM obtem_centro_custo_contarazao USING p_codtrp 'C' it_zib_contabil-hkont
                                             CHANGING it_zib_contabil-kostl.
        ENDIF.

        IF l_exige_material = 'X'.
          it_zib_contabil-matnr = wa_j_1bnflin-matnr.
        ELSE.
          CLEAR: it_zib_contabil-matnr.
        ENDIF.

        APPEND it_zib_contabil.

*
        it_zib_contabil-seqitem = it_zib_contabil-seqitem + 1.
        it_zib_contabil-bschl   = wa_zlest0018-chvlancto_c.
        it_zib_contabil-umskz   = wa_zlest0018-razaoesp_c.

        it_tipo_conta-seqitem = it_zib_contabil-seqitem.
        it_tipo_conta-tipo    = wa_zlest0018-tipoconta_c.
        it_tipo_conta-tipoch  = 'C'.
        APPEND it_tipo_conta.

        CASE wa_zlest0018-tipoconta_c.
          WHEN 'FA'.

            CASE wa_zlest0025-tp_atribuicao_c.
              WHEN '01'.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
              WHEN '02'.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
              WHEN OTHERS.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
            ENDCASE.

            it_zib_contabil-hkont = wa_lote-cd_adiministra.
            it_zib_contabil-gsber = wa_lote-branch.
          WHEN 'FS'.

            CASE wa_zlest0025-tp_atribuicao_c.
              WHEN '01'.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
              WHEN '02'.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
              WHEN OTHERS.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
            ENDCASE.

            it_zib_contabil-hkont = wa_zcte_trans-proprietario.
            it_zib_contabil-gsber = wa_lote-branch.
          WHEN 'RZ'.

            CASE wa_zlest0025-tp_atribuicao_c.
              WHEN '01'.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
              WHEN '02'.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
              WHEN OTHERS.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
            ENDCASE.

            it_zib_contabil-hkont = wa_zlest0018-contacredito.

            IF ( var_tomador EQ 'I' ).
              lc_centro = wa_lote-branch.
            ELSE.
              lc_centro = wa_lips-werks.
            ENDIF.


            CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
              EXPORTING
                centro               = lc_centro
              IMPORTING
                centro_out           = lc_centro
              EXCEPTIONS
                informar_centro      = 1
                nao_centro_r_virtual = 2
                informar_centro_out  = 3
                informar_centro_v    = 4
                OTHERS               = 5.
            it_zib_contabil-gsber = lc_centro.
        ENDCASE.

        it_zib_contabil-bupla = it_zib_contabil-gsber.
*
        CLEAR: it_zib_contabil-kostl, l_exige_ccusto, l_exige_material.
        PERFORM check_contarazao_exige_ccusto USING it_zib_contabil-hkont 'C' it_zib_contabil-bschl
                                           CHANGING l_exige_ccusto l_exige_material.
        IF l_exige_ccusto = 'X'.
          p_codtrp = wa_lote-branch.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = p_codtrp
            IMPORTING
              output = p_codtrp.
          PERFORM obtem_centro_custo_contarazao USING p_codtrp 'C' it_zib_contabil-hkont
                                             CHANGING it_zib_contabil-kostl.
        ENDIF.

        IF l_exige_material = 'X'.
          it_zib_contabil-matnr = wa_j_1bnflin-matnr.
        ELSE.
          CLEAR: it_zib_contabil-matnr.
        ENDIF.

        APPEND it_zib_contabil.


        wa_lote_item_ag-status         = c_g.
        wa_lote_item_ag-obj_key        = it_zib_contabil-obj_key.
        wa_lote_item_ag-ds_usuario_ctb = sy-uname.
      ENDIF.

      "*************************************************************************************
      "*************************************************************************************
      " Ajustes de Lote A Menos e a Maior
      "*************************************************************************************

      IF wa_lote_item_ag-vl_diferenca NE 0.

        CASE wa_lote_item_ag-chvid.
          WHEN '30'.
            "Quebra
            IF wa_lote_item_ag-vl_diferenca GT 0.
              wa_zlest0025-chvid = '32'.
              vg_chave_branco    = 'C'.
            ELSEIF wa_lote_item_ag-vl_diferenca LT 0.
              wa_zlest0025-chvid = '33'.
              vg_chave_branco    = 'D'.
            ENDIF.
          WHEN '31'.
            "Perda
            IF wa_lote_item_ag-vl_diferenca GT 0.
              wa_zlest0025-chvid = '34'.
              vg_chave_branco    = 'C'.
            ELSEIF wa_lote_item_ag-vl_diferenca LT 0.
              wa_zlest0025-chvid = '35'.
              vg_chave_branco    = 'D'.
            ENDIF.
        ENDCASE.

        SELECT SINGLE * INTO wa_zlest0025
          FROM zlest0025
         WHERE chvid EQ wa_zlest0025-chvid.

        IF NOT sy-subrc IS INITIAL.
          MESSAGE e020 WITH wa_zlest0025-chvid RAISING conta_deb_cred.
        ENDIF.

        SELECT SINGLE * INTO wa_zlest0018
          FROM zlest0018
         WHERE chvid       EQ wa_zlest0025-chvid
           AND tipcontabil EQ tip_contabil
           AND ctlglancto  EQ wa_zlest0025-ctlglancto
           AND bukrs       EQ wa_lote-bukrs
           AND tp_tomador  EQ var_tomador.

        IF NOT sy-subrc IS INITIAL.
          MESSAGE e020 WITH wa_zlest0025-chvid tip_contabil RAISING conta_deb_cred.
        ENDIF.

        CONCATENATE p_nm_lote p_cd_lote_finan INTO it_zib_contabil-obj_key.

        it_zib_contabil-seqitem = it_zib_contabil-seqitem + 1.
        it_zib_contabil-bschl   = wa_zlest0018-chvlancto_d.
        it_zib_contabil-umskz   = wa_zlest0018-razaoesp_d.
        it_zib_contabil-wrbtr   = abs( wa_lote_item_ag-vl_diferenca ).
        it_zib_contabil-dmbtr   = abs( wa_lote_item_ag-vl_diferenca ).
        it_zib_contabil-bupla   = it_zib_contabil-gsber.

        CONCATENATE wa_zlest0025-deschvid
                    'CTR' wa_lote_item_ag-nucontrato
                    'DACTE' wa_lote_item_ag-ctenum
               INTO it_zib_contabil-sgtxt SEPARATED BY space.
        CONCATENATE it_zib_contabil-sgtxt '/' wa_lote_item_ag-cteserie INTO it_zib_contabil-sgtxt.

        it_tipo_conta-seqitem = it_zib_contabil-seqitem.
        it_tipo_conta-tipo    = wa_zlest0018-tipoconta_d.
        it_tipo_conta-tipoch  = 'D'.
        APPEND it_tipo_conta.

        CASE wa_zlest0018-tipoconta_d.
          WHEN 'FA'.

            CASE wa_zlest0025-tp_atribuicao_d.
              WHEN '01'.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
              WHEN '02'.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
              WHEN OTHERS.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
            ENDCASE.

            it_zib_contabil-hkont = wa_lote-cd_adiministra.
            it_zib_contabil-gsber = wa_lote-branch.
          WHEN 'FS'.

            CASE wa_zlest0025-tp_atribuicao_d.
              WHEN '01'.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
              WHEN '02'.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
              WHEN OTHERS.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
            ENDCASE.

            it_zib_contabil-hkont = wa_zcte_trans-proprietario.
            it_zib_contabil-gsber = wa_lote-branch.
          WHEN 'RZ'.

            CASE wa_zlest0025-tp_atribuicao_d.
              WHEN '01'.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
              WHEN '02'.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
              WHEN OTHERS.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
            ENDCASE.

            it_zib_contabil-hkont = wa_zlest0018-contadebito.

            IF ( var_tomador EQ 'I' ).
              lc_centro             = wa_lote-branch.
            ELSE.
              lc_centro          = wa_lips-werks.
            ENDIF.

            CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
              EXPORTING
                centro               = lc_centro
              IMPORTING
                centro_out           = lc_centro
              EXCEPTIONS
                informar_centro      = 1
                nao_centro_r_virtual = 2
                informar_centro_out  = 3
                informar_centro_v    = 4
                OTHERS               = 5.
            it_zib_contabil-gsber   = lc_centro.
        ENDCASE.

        IF vg_chave_branco NE 'D'.
          CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
        ELSE.
          CLEAR: it_zib_contabil-zuonr.
          wa_lote_item_ag-nm_item_ctb_or = it_zib_contabil-seqitem.
        ENDIF.
        CONCATENATE wa_lote_item_ag-ctenum '-' wa_lote_item_ag-cteserie INTO it_zib_contabil-xblnr.

        CLEAR: it_zib_contabil-kostl, l_exige_ccusto, l_exige_material.
        PERFORM check_contarazao_exige_ccusto USING it_zib_contabil-hkont 'C' it_zib_contabil-bschl
                                           CHANGING l_exige_ccusto l_exige_material.
        IF l_exige_ccusto = 'X'.
          p_codtrp = wa_lote-branch.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = p_codtrp
            IMPORTING
              output = p_codtrp.
          PERFORM obtem_centro_custo_contarazao USING p_codtrp 'C' it_zib_contabil-hkont
                                             CHANGING it_zib_contabil-kostl.
        ENDIF.

        IF l_exige_material = 'X'.
          it_zib_contabil-matnr = wa_j_1bnflin-matnr.
        ELSE.
          CLEAR: it_zib_contabil-matnr.
        ENDIF.

        APPEND it_zib_contabil.


        it_zib_contabil-seqitem = it_zib_contabil-seqitem + 1.
        it_zib_contabil-bschl   = wa_zlest0018-chvlancto_c.
        it_zib_contabil-umskz   = wa_zlest0018-razaoesp_c.
        it_zib_contabil-bupla   = it_zib_contabil-gsber.

        it_tipo_conta-seqitem = it_zib_contabil-seqitem.
        it_tipo_conta-tipo    = wa_zlest0018-tipoconta_c.
        it_tipo_conta-tipoch  = 'C'.
        APPEND it_tipo_conta.

        CASE wa_zlest0018-tipoconta_c.
          WHEN 'FA'.

            CASE wa_zlest0025-tp_atribuicao_c.
              WHEN '01'.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
              WHEN '02'.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
              WHEN OTHERS.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
            ENDCASE.

            it_zib_contabil-hkont = wa_lote-cd_adiministra.
            it_zib_contabil-gsber = wa_lote-branch.
          WHEN 'FS'.

            CASE wa_zlest0025-tp_atribuicao_c.
              WHEN '01'.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
              WHEN '02'.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
              WHEN OTHERS.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
            ENDCASE.

            it_zib_contabil-hkont = wa_zcte_trans-proprietario.
            it_zib_contabil-gsber = wa_lote-branch.
          WHEN 'RZ'.

            CASE wa_zlest0025-tp_atribuicao_c.
              WHEN '01'.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
              WHEN '02'.
                CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
              WHEN OTHERS.
                CONCATENATE 'FR-' p_nm_lote INTO it_zib_contabil-zuonr.
            ENDCASE.

            it_zib_contabil-hkont = wa_zlest0018-contacredito.
            IF ( var_tomador EQ 'I' ).
              lc_centro = wa_lote-branch.
            ELSE.
              lc_centro = wa_lips-werks.
            ENDIF.

            CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
              EXPORTING
                centro               = lc_centro
              IMPORTING
                centro_out           = lc_centro
              EXCEPTIONS
                informar_centro      = 1
                nao_centro_r_virtual = 2
                informar_centro_out  = 3
                informar_centro_v    = 4
                OTHERS               = 5.
            it_zib_contabil-gsber = lc_centro.
        ENDCASE.

        IF vg_chave_branco NE 'C'.
          CONCATENATE 'FR-' wa_lote_item-nucontrato INTO it_zib_contabil-zuonr.
        ELSE.
          CLEAR: it_zib_contabil-zuonr.
          wa_lote_item_ag-nm_item_ctb_or = it_zib_contabil-seqitem.
        ENDIF.

        CLEAR: it_zib_contabil-kostl, l_exige_ccusto, l_exige_material.
        PERFORM check_contarazao_exige_ccusto USING it_zib_contabil-hkont 'C' it_zib_contabil-bschl
                                           CHANGING l_exige_ccusto l_exige_material.
        IF l_exige_ccusto = 'X'.
          p_codtrp = wa_lote-branch.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = p_codtrp
            IMPORTING
              output = p_codtrp.
          PERFORM obtem_centro_custo_contarazao USING p_codtrp 'C' it_zib_contabil-hkont
                                             CHANGING it_zib_contabil-kostl.
        ENDIF.

        IF l_exige_material = 'X'.
          it_zib_contabil-matnr = wa_j_1bnflin-matnr.
        ELSE.
          CLEAR: it_zib_contabil-matnr.
        ENDIF.

        APPEND it_zib_contabil.

        "*************************************************************************************
      ENDIF.

      APPEND wa_lote_item_ag TO it_lote_item_ag.
    ENDLOOP.
  ENDLOOP.
  "---------------------------------------------------------------------------------------------------
  "---------------------------------------------------------------------------------------------------

  APPEND wa_lote_item TO it_lote_item_ag.

  MODIFY zpfe_lote_item FROM TABLE it_lote_item_ag.

  "Montar a partir da ZIB_CONTABIL para gerar Documento GL de partidas no conta razão
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  "MODIFY ZIB_CONTABIL FROM TABLE IT_ZIB_CONTABIL.
  "COMMIT WORK.

  READ TABLE it_zib_contabil INDEX 1.
  IF sy-subrc IS INITIAL.

    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system = gd_documentheader-obj_sys.

    CONCATENATE it_zib_contabil-budat+6(4) it_zib_contabil-budat+3(2) it_zib_contabil-budat+0(2) INTO vbudat.
    CONCATENATE it_zib_contabil-bldat+6(4) it_zib_contabil-bldat+3(2) it_zib_contabil-bldat+0(2) INTO vbldat.

    gd_documentheader-obj_type   = 'IDOC'.
    gd_documentheader-username   = sy-uname.
    gd_documentheader-pstng_date = vbudat.
    gd_documentheader-bus_act    = 'RFBU'.

    gd_documentheader-obj_key    = it_zib_contabil-obj_key.
    gd_documentheader-header_txt = it_zib_contabil-bktxt.
    gd_documentheader-comp_code  = it_zib_contabil-bukrs.

    gd_documentheader-doc_date   = vbldat.
    gd_documentheader-fisc_year  = it_zib_contabil-gjahr.
    gd_documentheader-fis_period = it_zib_contabil-monat.
    gd_documentheader-doc_type   = it_zib_contabil-blart.
    gd_documentheader-ref_doc_no = it_zib_contabil-xblnr.

    "Procura fornecedor do Serviço
    READ TABLE it_tipo_conta WITH KEY tipo = 'FA'.
    IF sy-subrc IS NOT INITIAL.
      READ TABLE it_tipo_conta WITH KEY tipo = 'FS'.
    ENDIF.

    "Achou fornecedor do Serviço
    IF sy-subrc IS INITIAL.
      READ TABLE it_zib_contabil WITH KEY seqitem = it_tipo_conta-seqitem.
      SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE lifnr EQ it_zib_contabil-hkont.
    ENDIF.

    vg_posnr_acc = 1.
    LOOP AT it_zib_contabil.

      READ TABLE it_tipo_conta WITH KEY seqitem = it_zib_contabil-seqitem.

      CASE it_tipo_conta-tipo.
        WHEN 'FA' OR 'FS'.
          "Item do fornecedor
          CLEAR wa_payable.
          wa_payable-itemno_acc    = vg_posnr_acc.
          wa_payable-vendor_no     = it_zib_contabil-hkont.
          wa_payable-ref_key_1     = ''.
          wa_payable-ref_key_2     = ''.
          wa_payable-ref_key_3     = ''.
          wa_payable-bline_date    = vbudat.
          wa_payable-pymt_meth     = 'U'.
          wa_payable-pmnt_block    = 'A'.
          wa_payable-alloc_nmbr    = it_zib_contabil-zuonr.
          wa_payable-item_text     = it_zib_contabil-sgtxt.
          wa_payable-sp_gl_ind     = it_zib_contabil-umskz.
          wa_payable-bus_area      = it_zib_contabil-gsber.
          wa_payable-partner_bk    = it_zib_contabil-bvtyp.
          wa_payable-bank_id       = it_zib_contabil-hbkid.
          APPEND wa_payable TO it_payable.

          CLEAR wa_currencyamount.
          wa_currencyamount-itemno_acc  = vg_posnr_acc.
          wa_currencyamount-curr_type   = '00'.
          wa_currencyamount-currency    = it_zib_contabil-waers.
          IF it_tipo_conta-tipoch  = 'C'.
            wa_currencyamount-amt_doccur  = it_zib_contabil-wrbtr * -1.
          ELSE.
            wa_currencyamount-amt_doccur  = it_zib_contabil-wrbtr.
          ENDIF.
          APPEND wa_currencyamount TO it_currencyamount.
          wa_currencyamount-curr_type   = '10'.
          APPEND wa_currencyamount TO it_currencyamount.

        WHEN 'RZ'.
          "Item CtaRazão
          CLEAR wa_accountgl.
          wa_accountgl-costcenter     = it_zib_contabil-kostl.
          wa_accountgl-itemno_acc     = vg_posnr_acc.
          wa_accountgl-gl_account     = it_zib_contabil-hkont.
          wa_accountgl-ref_key_1      = ''.
          wa_accountgl-ref_key_2      = ''.
          wa_accountgl-ref_key_3      = ''.
          wa_accountgl-acct_type      = 'S'.
          wa_accountgl-item_text      = it_zib_contabil-sgtxt.
          wa_accountgl-bus_area       = it_zib_contabil-gsber.
          wa_accountgl-alloc_nmbr     = it_zib_contabil-zuonr.
          "WA_ACCOUNTGL-TAX_CODE       = 'I0'.
          wa_accountgl-doc_type       = it_zib_contabil-blart.
          wa_accountgl-taxjurcode     = wa_lfa1-txjcd.
          wa_accountgl-trade_id       = wa_lfa1-vbund.
          wa_accountgl-cs_trans_t     = ''.
          wa_accountgl-orderid        = ''.
          APPEND wa_accountgl TO it_accountgl.

          IF it_zib_contabil-matnr IS NOT INITIAL.
            it_criteria-itemno_acc    = vg_posnr_acc.
            it_criteria-fieldname     = 'ARTNR'.
*---> 13/06/2023 - Migração S4 - JS
*            it_criteria-character     = it_zib_contabil-matnr.
            it_criteria-character = CONV #( it_zib_contabil-matnr ).
*<--- 13/06/2023 - Migração S4 - JS

            APPEND it_criteria.
          ENDIF.

          CLEAR wa_currencyamount.
          wa_currencyamount-itemno_acc  = vg_posnr_acc.
          wa_currencyamount-curr_type   = '00'.
          wa_currencyamount-currency    = it_zib_contabil-waers.
          IF it_tipo_conta-tipoch  = 'C'.
            wa_currencyamount-amt_doccur  = it_zib_contabil-wrbtr * -1.
          ELSE.
            wa_currencyamount-amt_doccur  = it_zib_contabil-wrbtr.
          ENDIF.
          APPEND wa_currencyamount TO it_currencyamount.
          wa_currencyamount-curr_type   = '10'.
          APPEND wa_currencyamount TO it_currencyamount.

      ENDCASE.

      ADD 1 TO vg_posnr_acc.

    ENDLOOP.

    DATA: wa_zib_contabil_chv LIKE zib_contabil_chv.
    wa_zib_contabil_chv-mandt   = sy-mandt.
    wa_zib_contabil_chv-obj_key = gd_documentheader-obj_key.
    INSERT INTO zib_contabil_chv VALUES wa_zib_contabil_chv.

    IF sy-subrc IS INITIAL.
      sy-tcode = 'FB05'.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST' "#EC CI_USAGE_OK[2438131]
        EXPORTING                            "#EC CI_USAGE_OK[2628704]
          documentheader = gd_documentheader
        IMPORTING
          obj_type       = wa_returnobj-obj_type
          obj_key        = wa_returnobj-obj_key
          obj_sys        = wa_returnobj-obj_sys
        TABLES
          criteria       = it_criteria
          accountgl      = it_accountgl
          accountpayable = it_payable
          currencyamount = it_currencyamount
          return         = it_bapiret.
    ELSE.

      SELECT SINGLE *
        INTO wa_zib_contabil_chv
        FROM zib_contabil_chv
       WHERE obj_key EQ gd_documentheader-obj_key.

      wa_bapiret-type	= 'E'.
      wa_bapiret-id   = 'ZPK'.
      wa_bapiret-number = '003'.
      CONCATENATE 'CHAVE DUPLICADA' ': - Este documento já foi enviado ao SAP' INTO wa_bapiret-message.
      CONCATENATE wa_bapiret-message ' Doc: ' wa_zib_contabil_chv-belnr ' Empresa: ' wa_zib_contabil_chv-bukrs ' Ano: ' wa_zib_contabil_chv-gjahr
             INTO wa_bapiret-message.
      wa_bapiret-message_v1 = wa_returnobj-obj_type.
      wa_bapiret-message_v2 = wa_returnobj-obj_key.
      wa_bapiret-message_v3 = wa_returnobj-obj_sys.
      wa_bapiret-message_v4 = 'Para enviar novamente deve ser incrementado 1 no ID_ENVIADO_SAP'.
      APPEND wa_bapiret TO it_bapiret.
    ENDIF.

    READ TABLE it_bapiret INTO wa_bapiret WITH KEY type = 'E'.
    IF sy-subrc IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      COMMIT WORK.

      UPDATE bkpf SET tcode = 'FB05'
                WHERE awtyp EQ wa_returnobj-obj_type
                  AND awkey EQ wa_returnobj-obj_key.

      COMMIT WORK.

      SELECT awtyp awkey awsys belnr bukrs gjahr
        FROM bkpf UP TO 1 ROWS
        INTO (vg_awtyp, vg_awkey, vg_awsys,
              vg_belnr, vg_bukrs, vg_gjahr)
       WHERE ( awtyp EQ wa_returnobj-obj_type )
         AND ( awkey EQ wa_returnobj-obj_key  )
        ORDER BY awtyp awkey awsys.
      ENDSELECT.

      UPDATE zib_contabil_chv
         SET belnr = vg_belnr
             bukrs = vg_bukrs
             gjahr = vg_gjahr
       WHERE obj_key EQ wa_returnobj-obj_key.

      CLEAR wa_bapiret.
      wa_bapiret-type   = 'S'.
      wa_bapiret-id     = 'Z01'.
      wa_bapiret-number = '003'.
      wa_bapiret-message_v1 = vg_belnr.
      wa_bapiret-message_v2 = vg_bukrs.
      wa_bapiret-message_v3 = vg_gjahr.

      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          language = 'P'
          msg_id   = 'Z01'
          msg_no   = '003'
          msg_var1 = wa_bapiret-message_v1
          msg_var2 = wa_bapiret-message_v2
          msg_var3 = wa_bapiret-message_v3
        IMPORTING
          msg_text = wa_bapiret-message.

      APPEND wa_bapiret TO it_bapiret.

    ELSE.
      DELETE FROM zib_contabil_chv  WHERE obj_key EQ gd_documentheader-obj_key.
    ENDIF.

    LOOP AT it_bapiret INTO wa_bapiret.
      CLEAR wa_return.
      MOVE-CORRESPONDING wa_bapiret TO wa_return.
      APPEND wa_return TO it_return.
    ENDLOOP.

    CLEAR: vg_belnr.
    seqimsg = 0.
    CONCATENATE p_nm_lote p_cd_lote_finan INTO it_zib_contabil-obj_key.
    LOOP AT it_return INTO wa_return.

      ADD 1 TO seqimsg.

      IF ( wa_return-type EQ 'S' AND wa_return-id = 'Z01' AND wa_return-number EQ '003' ).
        vg_belnr = wa_return-message_v1(10).

        SELECT * INTO TABLE it_lote_item_msg
          FROM zpfe_lote_item
         WHERE obj_key EQ it_zib_contabil-obj_key.

        IF sy-subrc IS INITIAL.
          LOOP AT it_lote_item_msg.
            it_lote_item_msg-status   =  'C'.
            it_lote_item_msg-belnr    = wa_return-message_v1(10).
            it_lote_item_msg-gjahr    = wa_return-message_v3(04).
            it_lote_item_msg-dt_belnr = sy-datum.
            it_lote_item_msg-hr_belnr = sy-uzeit.
            MODIFY zpfe_lote_item FROM it_lote_item_msg.

            IF it_lote_item_msg-chvid EQ '2'.
              DATA: wa_ciot TYPE zcte_ciot.
              SELECT SINGLE * INTO wa_ciot
                FROM zcte_ciot
               WHERE docnum EQ wa_lote_item-docnum.

              IF sy-subrc IS INITIAL.
                wa_ciot-st_ciot = '6'.
                MODIFY zcte_ciot FROM wa_ciot.
              ENDIF.
            ENDIF.

            DELETE it_lote_item WHERE nm_lote_item EQ it_lote_item_msg-nm_lote_item.
            COMMIT WORK.
          ENDLOOP.
        ENDIF.
      ENDIF.

      wa_msg_lote-obj_key    = it_zib_contabil-obj_key.
      wa_msg_lote-seqmsg     = seqimsg.
      wa_msg_lote-type       = wa_return-type.
      wa_msg_lote-id         = wa_return-id.
      wa_msg_lote-numero     = wa_return-number.
      wa_msg_lote-message    = wa_return-message.
      wa_msg_lote-log_no     = wa_return-log_no.
      wa_msg_lote-log_msg_no = wa_return-log_msg_no.
      wa_msg_lote-message_v1 = wa_return-message_v1.
      wa_msg_lote-message_v2 = wa_return-message_v2.
      wa_msg_lote-message_v3 = wa_return-message_v3.
      wa_msg_lote-message_v4 = wa_return-message_v4.
      MODIFY zpfe_lote_msg FROM wa_msg_lote.

    ENDLOOP.

    IF vg_belnr IS INITIAL.
      UPDATE zpfe_lote_item
         SET status   =  'E'
       WHERE obj_key EQ it_zib_contabil-obj_key.
      COMMIT WORK.
    ENDIF.

  ENDIF.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  CALL FUNCTION 'Z_PFE_PSQ_ITENS'
    EXPORTING
      p_lote_alv  = wa_lote_alv
    TABLES
      p_itens     = it_lote_item_msg
      p_itens_alv = it_lotes_item_alv
    EXCEPTIONS
      sem_itens   = 1
      OTHERS      = 2.

  READ TABLE it_lotes_item     INTO wa_lote_item INDEX 1.
  READ TABLE it_lotes_item_alv INTO wa_lote_item_alv INDEX 1.

ENDFUNCTION.
