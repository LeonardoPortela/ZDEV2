*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: RODRIGO C.                                              &*
*& Data.....: 26/03/2024                                              &*
*& Descrição: JOB manutenção da tabela Notas de Produtor para Vincular&*
*& Transação: N/A Execução via JOB                                    &*
*&                                                                    &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A1XAW |25/10/2024 |Ajuste da fila de NF entrada - &*
*&                                    |Inclusão de NF de Entrada de   &*
*&                                    |Transferência. Chamado: 155660 &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A1XAW |11/11/2024 |Ajustar Campo CANCEL em Tabelas&*
*&                                    |ZSDTVINC_P_FLOTE,ZSDTPROD_FLOTE&*
*&                                    |e ZSDTFLOTE_FLOTE.             &*
*&                                    |Chamado: 157683.               &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A1XAW |23/11/2024 |Ajustes no Processamento de    &*
*&                                    |cancelamento de notas de saida.&*
*&                                    |Chamado: 157919.               &*
*&--------------------------------------------------------------------&*
REPORT zsdr0012.

*----------------------------------------------------------------------*
* Tabelas                                                              *
*----------------------------------------------------------------------*
TABLES: j_1bnfdoc, j_1bnflin.

*----------------------------------------------------------------------*
* Types                                                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_nf_entradas,
         docnum TYPE j_1bnflin-docnum,
         itmnum TYPE j_1bnflin-itmnum,
         matnr  TYPE j_1bnflin-matnr,
         cfop   TYPE j_1bnflin-cfop,
         menge  TYPE j_1bnflin-menge,
         werks  TYPE j_1bnflin-werks,
         matkl  TYPE j_1bnflin-matkl,
         docdat TYPE j_1bnfdoc-docdat,
         cancel TYPE j_1bnfdoc-cancel,
       END OF ty_nf_entradas,

       BEGIN OF ty_nf_canceladas,
         docnum TYPE j_1bnflin-docnum,
         itmnum TYPE j_1bnflin-itmnum,
         matnr  TYPE j_1bnflin-matnr,
         cfop   TYPE j_1bnflin-cfop,
         menge  TYPE j_1bnflin-menge,
         werks  TYPE j_1bnflin-werks,
         matkl  TYPE j_1bnflin-matkl,
         docdat TYPE j_1bnfdoc-docdat,
         cancel TYPE j_1bnfdoc-cancel,
       END OF ty_nf_canceladas,

       BEGIN OF ty_zsdt_depara_cen,
         vkorg       TYPE zsdt_depara_cen-vkorg,
         centro_real TYPE zsdt_depara_cen-centro_real,
         centrov_1   TYPE zsdt_depara_cen-centrov_1,
       END OF ty_zsdt_depara_cen,

       BEGIN OF ty_werks,
         werks TYPE j_1bnflin-werks,
       END OF ty_werks,

       BEGIN OF ty_check_zsdtprod_flote,
         docnum           TYPE zsdtprod_flote-docnum,
         itmnum           TYPE zsdtprod_flote-itmnum,
         werks            TYPE zsdtprod_flote-werks,
         seq              TYPE zseq,
         qtd_nf           TYPE zsdtprod_flote-qtd_nf,
         saldo_disponivel TYPE zsdtprod_flote-saldo_disponivel,
       END OF ty_check_zsdtprod_flote,

       BEGIN OF ty_vinclote,
         docnum_flote TYPE zsdtvinc_p_flote-docnum_flote,
         docnum_eprod TYPE zsdtvinc_p_flote-docnum_eprod,
       END OF ty_vinclote,

       BEGIN OF ty_prod_flote,
         docnum        TYPE zsdtprod_flote-docnum,
         compra_fim_es TYPE zsdtprod_flote-compra_fim_es,
       END OF ty_prod_flote.

*----------------------------------------------------------------------*
* Tabela interna                                                       *
*----------------------------------------------------------------------*
DATA: ti_nf_entradas          TYPE TABLE OF ty_nf_entradas,
      wa_nf_entradas          TYPE ty_nf_entradas,
      ti_nf_canceladas        TYPE TABLE OF ty_nf_canceladas,
      wa_nf_canceladas        TYPE ty_nf_canceladas,
      ti_check_zsdtprod_flote TYPE TABLE OF ty_check_zsdtprod_flote,
      wa_check_zsdtprod_flote TYPE ty_check_zsdtprod_flote,
      ti_zsdtprod_flote       TYPE TABLE OF zsdtprod_flote,
      wa_zsdtprod_flote       TYPE zsdtprod_flote,
      ti_zsdtvinc_p_flote     TYPE TABLE OF zsdtvinc_p_flote,
      wa_zsdtvinc_p_flote     TYPE zsdtvinc_p_flote,
      tg_flote_flote          TYPE TABLE OF zsdtflote_flote,
      tg_fl_flote_ref         TYPE TABLE OF zsdtfl_flote_ref,
      ti_zsdt_depara_cen      TYPE TABLE OF ty_zsdt_depara_cen,
      wa_zsdt_depara_cen      TYPE ty_zsdt_depara_cen,
      ti_werks                TYPE TABLE OF ty_werks,
      wa_werks                TYPE ty_werks,
      git_docnum_rom_completo TYPE zsdt0001_docnum_t,
      ti_vinclote             TYPE TABLE OF ty_vinclote,
      ti_prod_flote           TYPE TABLE OF ty_prod_flote.

*----------------------------------------------------------------------*
* Ranges                                                               *
*----------------------------------------------------------------------*
DATA: rg_cfop             TYPE RANGE OF j_1bnflin-cfop,
      wa_cfop             LIKE LINE  OF rg_cfop,
      t_cfop_e_fins       TYPE TABLE OF rgsb4,
      w_cfop_e_fins       TYPE rgsb4,
      t_cfop_e_comerc     TYPE TABLE OF rgsb4,
      w_cfop_e_comerc     TYPE rgsb4,
      t_cfop_form_lote    TYPE TABLE OF rgsb4,
      w_cfop_form_lote    TYPE rgsb4,
      t_matkl             TYPE TABLE OF rgsb4,
      w_matkl             TYPE rgsb4,
      v_id_referencia_rom TYPE zsdt0001-id_referencia.

RANGES: r_cfop_e_fins    FOR j_1bnflin-cfop,
        r_cfop_e_comerc  FOR j_1bnflin-cfop,
        r_cfop_form_lote FOR j_1bnflin-cfop,
        r_cfop_todos     FOR j_1bnflin-cfop,
        r_matkl          FOR j_1bnflin-matkl.

DATA gr_cfop_ent_trns TYPE RANGE OF j_1bcfop.

*----------------------------------------------------------------------*
* Tela de seleção                                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_docdat FOR j_1bnfdoc-docdat,
                  s_docnum FOR j_1bnfdoc-docnum, "SD - Ajuste Insert Fila 1x1 - Processo Fob US 136393 - WPP
                  s_cfop   FOR j_1bnflin-cfop,
                  s_cfop_f FOR j_1bnflin-cfop, "CFOP formação de lote
                  s_branch FOR j_1bnfdoc-branch.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  "trata inicialização de váriais para o filtro
  PERFORM inicializa_variaiveis.

  PERFORM: seleciona_notas_entradas, "Seleciona notas fiscais que foram realizadas entradas
           update_nf_entradas.       "Atualiza notas fiscais na Tabela ZSDTPROD_FLOTE

  PERFORM: f_sel_cancel_nfe_saida,  "Seleção Cancelamento da NFe de Saida Form. Lote
           f_proc_cancel_nfe_saida. "Processamento Cancelamento da NFe de Saida Form. Lote

  PERFORM selec_notas_retorno_form_lote.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Form seleciona_notas_entradas
*&---------------------------------------------------------------------*
FORM seleciona_notas_entradas .

  TYPES: BEGIN OF ty_zsdt0001,
           bukrs        TYPE bukrs,
           branch       TYPE j_1bbranc_,
           nr_safra     TYPE znr_safra,
           tp_movimento TYPE ztp_mov,
           nr_refer_out TYPE zch_ref,
           nr_refer_in  TYPE zch_ref,
           nr_romaneio  TYPE znr_romaneio,
         END   OF ty_zsdt0001.

  DATA: lt_zsdt0001_e TYPE TABLE OF ty_zsdt0001.

  DATA: lv_candat TYPE sy-datum.

  IF s_cfop[] IS INITIAL.
    APPEND LINES OF r_cfop_todos TO s_cfop.
  ENDIF.

  SELECT li~docnum li~itmnum li~matnr li~cfop li~menge li~werks li~matkl dc~docdat dc~cancel
         FROM j_1bnfdoc AS dc INNER JOIN j_1bnflin AS li
         ON li~docnum EQ dc~docnum
         INTO TABLE ti_nf_entradas
         WHERE dc~pstdat IN s_docdat
         AND   dc~docnum in s_docnum "SD - Ajuste Insert Fila 1x1 - Processo Fob US 136393 - WPP
         AND   dc~direct EQ '1'
         AND   dc~model  EQ '55'
         AND   dc~doctyp EQ '1'
         AND   dc~branch IN s_branch
         AND   li~cfop   IN s_cfop
         AND   li~matkl  IN r_matkl
         AND   li~ownpro EQ space.

  SORT ti_nf_entradas BY docnum itmnum.

  IF ti_nf_entradas[] IS NOT INITIAL.

    " Seleciona Notas para validar se já foram atualizadas
    SELECT docnum itmnum werks seq qtd_nf saldo_disponivel
           FROM zsdtprod_flote
           INTO TABLE ti_check_zsdtprod_flote
           FOR ALL ENTRIES IN ti_nf_entradas
           WHERE docnum EQ ti_nf_entradas-docnum
             AND itmnum EQ ti_nf_entradas-itmnum
             AND cancel EQ space.

    SORT ti_check_zsdtprod_flote BY docnum itmnum werks.

    DATA(r_docnums) =  VALUE rsis_t_range( FOR lwa_nf_entradas IN ti_nf_entradas
                                           ( sign = 'I' option = 'EQ' low = lwa_nf_entradas-docnum )
                                          ).

    zcl_les_utils=>check_doc_fiscal_rom_completo( EXPORTING r_docnum              = r_docnums
                                                  IMPORTING e_docnum_rom_completo = git_docnum_rom_completo
                                                           ).

  ENDIF.

  LOOP AT ti_nf_entradas INTO wa_nf_entradas.
    wa_werks-werks = wa_nf_entradas-werks.
    APPEND wa_werks TO ti_werks.
  ENDLOOP.

  SORT ti_werks BY werks.
  DELETE ADJACENT DUPLICATES FROM ti_werks COMPARING werks.

  " Seleciona De-Para de Centros
  IF NOT ti_werks[] IS INITIAL.
    SELECT vkorg centro_real centrov_1
           FROM zsdt_depara_cen
           INTO TABLE ti_zsdt_depara_cen
           FOR ALL ENTRIES IN ti_werks
           WHERE centrov_1 EQ ti_werks-werks.
  ENDIF.

ENDFORM.

FORM f_sel_cancel_nfe_saida.

  IF s_cfop_f[] IS INITIAL.
    APPEND LINES OF r_cfop_form_lote TO s_cfop_f.
  ENDIF.

  " Retirada do Saldo das tabelas:  (Cancelamento da NFe de formação de lote)
  SELECT li~docnum li~itmnum li~matnr li~cfop li~menge li~werks li~matkl dc~docdat dc~cancel
         FROM j_1bnfdoc AS dc INNER JOIN j_1bnflin AS li
         ON li~docnum EQ dc~docnum
         INTO TABLE ti_nf_canceladas
         WHERE dc~candat IN s_docdat
         AND   dc~docnum in s_docnum "SD - Ajuste Insert Fila 1x1 - Processo Fob US 136393 - WPP
         AND   dc~direct EQ '2'
         AND   dc~model  EQ '55'
         AND   dc~cancel EQ abap_true
         AND   dc~doctyp EQ '1'
         AND   dc~branch IN s_branch
         AND   li~cfop   IN s_cfop_f.

  CHECK ti_nf_canceladas[] IS NOT INITIAL.

  SORT ti_nf_canceladas BY docnum itmnum.

  SELECT *
    FROM zsdtvinc_p_flote
    INTO TABLE ti_zsdtvinc_p_flote
    FOR ALL ENTRIES IN ti_nf_canceladas
    WHERE docnum_flote EQ ti_nf_canceladas-docnum
      AND cancel       EQ space.

  CHECK ti_zsdtvinc_p_flote[] IS NOT INITIAL.

  SORT ti_zsdtvinc_p_flote BY docnum_eprod.

  SELECT *
    FROM zsdtprod_flote
    INTO TABLE ti_zsdtprod_flote
    FOR ALL ENTRIES IN ti_zsdtvinc_p_flote
    WHERE docnum EQ ti_zsdtvinc_p_flote-docnum_eprod
      AND cancel EQ space.

  SORT ti_zsdtprod_flote BY docnum.

  SELECT *
    FROM zsdtflote_flote INTO TABLE tg_flote_flote
    FOR ALL ENTRIES IN ti_zsdtvinc_p_flote
  WHERE docnum EQ ti_zsdtvinc_p_flote-docnum_eprod
    AND cancel EQ space.

  IF sy-subrc IS INITIAL.
    SORT tg_flote_flote BY docnum.
  ENDIF.

  SELECT * FROM zsdtfl_flote_ref
    INTO TABLE tg_fl_flote_ref
    FOR ALL ENTRIES IN ti_zsdtvinc_p_flote
  WHERE docnum_flote EQ ti_zsdtvinc_p_flote-docnum_ref
    AND docnum_eprod EQ ti_zsdtvinc_p_flote-docnum_eprod
    AND cancel       EQ space.

  IF sy-subrc IS INITIAL.
    SORT tg_fl_flote_ref BY docnum_flote docnum_eprod.

  ENDIF.

ENDFORM.

FORM f_proc_cancel_nfe_saida.

  " Retirada do Saldo das tabelas:  (Cancelamento da NFe de formação de lote)
  LOOP AT ti_zsdtvinc_p_flote INTO wa_zsdtvinc_p_flote WHERE docnum_ref IS INITIAL.
    DATA(vl_qtd_vinc)  = wa_zsdtvinc_p_flote-qtd_vinc.
    DATA(vl_qtd_vinc2) = vl_qtd_vinc.
* Marcar registro para cancelamento
    PERFORM zf_set_line_to_cancel USING wa_zsdtvinc_p_flote.
    UPDATE zsdtvinc_p_flote
       SET cancel    = wa_zsdtvinc_p_flote-cancel
           us_cancel = wa_zsdtvinc_p_flote-us_cancel
           dt_cancel = wa_zsdtvinc_p_flote-dt_cancel
           hr_cancel = wa_zsdtvinc_p_flote-hr_cancel
    WHERE docnum_flote EQ wa_zsdtvinc_p_flote-docnum_flote
      AND docnum_eprod EQ wa_zsdtvinc_p_flote-docnum_eprod
      AND id_vinc      EQ wa_zsdtvinc_p_flote-id_vinc.
* Efetiva ou retorno manutenção de tabela transparente
    PERFORM zf_commit_or_rollback_tbl USING sy-subrc.

    IF wa_zsdtvinc_p_flote-vinc_virtual IS INITIAL.
* Processamento da Tabela Notas de Produtor para Vincular (ZSDTPROD_FLOTE).
      LOOP AT ti_zsdtprod_flote INTO wa_zsdtprod_flote WHERE docnum EQ wa_zsdtvinc_p_flote-docnum_eprod.
* Marcar registro para cancelamento
        PERFORM zf_set_line_to_cancel USING wa_zsdtprod_flote.
        UPDATE zsdtprod_flote
           SET cancel    = wa_zsdtprod_flote-cancel
               us_cancel = wa_zsdtprod_flote-us_cancel
               dt_cancel = wa_zsdtprod_flote-dt_cancel
               hr_cancel = wa_zsdtprod_flote-hr_cancel
        WHERE docnum EQ wa_zsdtprod_flote-docnum
          AND itmnum EQ wa_zsdtprod_flote-itmnum
          AND werks  EQ wa_zsdtprod_flote-werks
          AND seq    EQ wa_zsdtprod_flote-seq.

        IF sy-subrc IS INITIAL.
          IF wa_zsdtprod_flote-saldo_vinc LE vl_qtd_vinc.
            DATA(vl_subtraendo) = wa_zsdtprod_flote-saldo_vinc.

          ELSE.
            vl_subtraendo = vl_qtd_vinc.

          ENDIF.

          wa_zsdtprod_flote-saldo_vinc           = wa_zsdtprod_flote-saldo_vinc - vl_subtraendo.
          wa_zsdtprod_flote-saldo_disponivel     = wa_zsdtprod_flote-saldo_disponivel + vl_subtraendo.
          wa_zsdtprod_flote-saldo_nao_disponivel = abap_false.
          vl_qtd_vinc                            = vl_qtd_vinc - vl_subtraendo.
* Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE, ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF.
          PERFORM zf_number_get_next USING    '01'
                                              'ZSEQPROD'
                                     CHANGING wa_zsdtprod_flote-seq.
* Marcar registro para criação de linha nova.
          PERFORM zf_set_line_to_new USING wa_zsdtprod_flote.
* Inseri o registro atualizado para a chave da tabela que foi marcada como cancelada anteriormente.
          INSERT zsdtprod_flote FROM wa_zsdtprod_flote.
* Efetiva ou retorno manutenção de tabela transparente
          PERFORM zf_commit_or_rollback_tbl USING sy-subrc.

        ENDIF.

        IF vl_qtd_vinc IS INITIAL.
          EXIT.

        ENDIF.

      ENDLOOP.

    ELSE.
      LOOP AT ti_zsdtvinc_p_flote INTO DATA(el_zsdtvinc_p_flote) WHERE docnum_flote EQ wa_zsdtvinc_p_flote-docnum_flote
                                                                   AND docnum_ref   EQ wa_zsdtvinc_p_flote-docnum_eprod.
* Marcar registro para cancelamento
        PERFORM zf_set_line_to_cancel USING el_zsdtvinc_p_flote.
        UPDATE zsdtvinc_p_flote
           SET cancel    = el_zsdtvinc_p_flote-cancel
               us_cancel = el_zsdtvinc_p_flote-us_cancel
               dt_cancel = el_zsdtvinc_p_flote-dt_cancel
               hr_cancel = el_zsdtvinc_p_flote-hr_cancel
        WHERE docnum_flote EQ el_zsdtvinc_p_flote-docnum_flote
          AND docnum_eprod EQ el_zsdtvinc_p_flote-docnum_eprod
          AND id_vinc      EQ el_zsdtvinc_p_flote-id_vinc.
* Efetiva ou retorno manutenção de tabela transparente
        PERFORM zf_commit_or_rollback_tbl USING sy-subrc.
* Processamento da Tabela auxiliar - Controle do Saldo do Liberado pelo Retorno (ZSDTFL_FLOTE_REF).
        READ TABLE tg_fl_flote_ref INTO DATA(el_fl_flote_ref) WITH KEY docnum_flote = el_zsdtvinc_p_flote-docnum_ref
                                                                       docnum_eprod = el_zsdtvinc_p_flote-docnum_eprod
                                                              BINARY SEARCH.

        IF sy-subrc IS INITIAL.
* Marcar registro para cancelamento
          PERFORM zf_set_line_to_cancel USING el_fl_flote_ref.
          UPDATE zsdtfl_flote_ref
             SET cancel    = el_fl_flote_ref-cancel
                 us_cancel = el_fl_flote_ref-us_cancel
                 dt_cancel = el_fl_flote_ref-dt_cancel
                 hr_cancel = el_fl_flote_ref-hr_cancel
          WHERE docnum_flote EQ el_fl_flote_ref-docnum_flote
            AND docnum_eprod EQ el_fl_flote_ref-docnum_eprod
            AND seq          EQ el_fl_flote_ref-seq.

          IF sy-subrc IS INITIAL.
* Marcar registro para criação de linha nova.
            PERFORM zf_set_line_to_new USING el_fl_flote_ref.

            IF el_fl_flote_ref-qtd_vinc LE vl_qtd_vinc2.
              vl_subtraendo = el_fl_flote_ref-qtd_vinc.

            ELSE.
              vl_subtraendo = vl_qtd_vinc2.

            ENDIF.

            el_fl_flote_ref-qtd_vinc         = el_fl_flote_ref-qtd_vinc - vl_subtraendo.
            el_fl_flote_ref-saldo_disponivel = el_fl_flote_ref-saldo_disponivel + vl_subtraendo.
            vl_qtd_vinc2                     = vl_qtd_vinc2 - vl_subtraendo.
* Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE, ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF.
            PERFORM zf_number_get_next USING    '01'
                                                'ZSEQFLREF'
                                       CHANGING el_fl_flote_ref-seq.
* Inseri o registro atualizado para a chave da tabela que foi marcada como cancelada anteriormente.
            INSERT zsdtfl_flote_ref FROM el_fl_flote_ref.
* Efetiva ou retorno manutenção de tabela transparente
            PERFORM zf_commit_or_rollback_tbl USING sy-subrc.

          ENDIF.

        ENDIF.

      ENDLOOP.
* Processamento da Tabela Notas de formação de lote Recusada para Vincular (ZSDTFLOTE_FLOTE).
      LOOP AT tg_flote_flote INTO DATA(el_flote_flote) WHERE docnum EQ wa_zsdtvinc_p_flote-docnum_eprod.
* Marcar registro para cancelamento
        PERFORM zf_set_line_to_cancel USING el_flote_flote.
        UPDATE zsdtflote_flote
           SET cancel    = el_flote_flote-cancel
               us_cancel = el_flote_flote-us_cancel
               dt_cancel = el_flote_flote-dt_cancel
               hr_cancel = el_flote_flote-hr_cancel
        WHERE docnum EQ el_flote_flote-docnum
          AND itmnum EQ el_flote_flote-itmnum
          AND seq    EQ el_flote_flote-seq.

        IF sy-subrc IS INITIAL.
* Marcar registro para criação de linha nova.
          PERFORM zf_set_line_to_new USING el_flote_flote.

          IF el_flote_flote-saldo_vinc LE vl_qtd_vinc.
            vl_subtraendo = el_flote_flote-saldo_vinc.

          ELSE.
            vl_subtraendo = vl_qtd_vinc.

          ENDIF.

          el_flote_flote-saldo_vinc           = el_flote_flote-saldo_vinc - vl_subtraendo.
          el_flote_flote-saldo_disponivel     = el_flote_flote-saldo_disponivel + vl_subtraendo.
          el_flote_flote-saldo_nao_disponivel = abap_false.
          vl_qtd_vinc                         = vl_qtd_vinc - vl_subtraendo.
* Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE, ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF.
          PERFORM zf_number_get_next USING    '01'
                                              'ZSEQFLOTE'
                                     CHANGING el_flote_flote-seq.
* Inseri o registro atualizado para a chave da tabela que foi marcada como cancelada anteriormente.
          INSERT zsdtflote_flote FROM el_flote_flote.
* Efetiva ou retorno manutenção de tabela transparente
          PERFORM zf_commit_or_rollback_tbl USING sy-subrc.

        ENDIF.

        IF vl_qtd_vinc IS INITIAL.
          EXIT.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form update_nf_entradas
*&---------------------------------------------------------------------*
FORM update_nf_entradas .

  " Atualiza notas que foram realizadas entradas para compor saldo
  LOOP AT ti_nf_entradas INTO wa_nf_entradas WHERE cancel = abap_false.
    CLEAR wa_zsdtprod_flote.

    READ TABLE ti_check_zsdtprod_flote INTO wa_check_zsdtprod_flote WITH KEY docnum = wa_nf_entradas-docnum
                                                                             itmnum = wa_nf_entradas-itmnum
                                                                             werks  = wa_nf_entradas-werks BINARY SEARCH.
    IF sy-subrc NE 0.

*"// INICIO WBARBOSA 100125 NÃO CONTINUA SE NÃO EXITIR ROMANEIO
      READ TABLE git_docnum_rom_completo INTO DATA(ls_docnum_rom_completo) WITH KEY docnum = wa_nf_entradas-docnum.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      IF ls_docnum_rom_completo-romaneio_completo IS NOT INITIAL.
        wa_zsdtprod_flote-romaneio_completo = abap_true.
      ENDIF.
*"// FIM WBARBOSA 100125 NÃO CONTINUA SE NÃO EXITIR ROMANEIO

      wa_zsdtprod_flote-mandt            = sy-mandt.
      wa_zsdtprod_flote-docnum           = wa_nf_entradas-docnum.
      wa_zsdtprod_flote-itmnum           = wa_nf_entradas-itmnum.
      wa_zsdtprod_flote-werks            = wa_nf_entradas-werks.
      wa_zsdtprod_flote-data_emissao     = wa_nf_entradas-docdat.
      wa_zsdtprod_flote-material         = wa_nf_entradas-matnr.
      wa_zsdtprod_flote-matkl            = wa_nf_entradas-matkl.

      CALL METHOD zcl_eudr_utils=>check_doc_fiscal_eudr
        EXPORTING
          i_docnum = wa_nf_entradas-docnum
        RECEIVING
          r_eudr   = DATA(lv_eudr).
      IF lv_eudr EQ 'S'.
        wa_zsdtprod_flote-eudr = abap_true.
      ENDIF.

      READ TABLE ti_zsdt_depara_cen INTO wa_zsdt_depara_cen WITH KEY centrov_1 = wa_nf_entradas-werks.
      IF sy-subrc EQ 0.
        wa_zsdtprod_flote-werks_real     = wa_zsdt_depara_cen-centro_real.
      ENDIF.

      wa_zsdtprod_flote-qtd_nf           = wa_nf_entradas-menge.
      wa_zsdtprod_flote-saldo_disponivel = wa_nf_entradas-menge.

      IF wa_nf_entradas-cfop IN r_cfop_e_fins.
        wa_zsdtprod_flote-compra_fim_es  = abap_true.
      ENDIF.

      IF wa_nf_entradas-cfop IN gr_cfop_ent_trns.
        wa_zsdtprod_flote-entrada_transf = abap_true.

      ENDIF.
* Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE, ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF.
      PERFORM zf_number_get_next USING    '01'
                                          'ZSEQPROD'
                                 CHANGING wa_zsdtprod_flote-seq.
* Marcar registro para criação de linha nova.
      PERFORM zf_set_line_to_new USING wa_zsdtprod_flote.

      MODIFY zsdtprod_flote FROM wa_zsdtprod_flote.
* Efetiva ou retorno manutenção de tabela transparente
      PERFORM zf_commit_or_rollback_tbl USING sy-subrc.

    ENDIF.

  ENDLOOP.

  " Retirar notas Canceladas da tabela de Controle: ZSDTPROD_FLOTE
  LOOP AT ti_nf_entradas INTO wa_nf_entradas WHERE cancel = abap_true.
    IF NOT wa_nf_entradas-docnum IS INITIAL AND NOT wa_nf_entradas-werks IS INITIAL.

      READ TABLE ti_check_zsdtprod_flote INTO wa_check_zsdtprod_flote WITH KEY docnum = wa_nf_entradas-docnum
                                                                               itmnum = wa_nf_entradas-itmnum
                                                                               werks  = wa_nf_entradas-werks BINARY SEARCH.
      " Eliminar Notas Canceladas se elas ainda não tiverem sido vinculada em nenhuma de formação de lote
      IF sy-subrc EQ 0 AND wa_check_zsdtprod_flote-qtd_nf EQ wa_check_zsdtprod_flote-saldo_disponivel.
* Marcar registro para cancelamento
        PERFORM zf_set_line_to_cancel USING wa_zsdtprod_flote.
        UPDATE zsdtprod_flote
           SET cancel    = wa_zsdtprod_flote-cancel
               us_cancel = wa_zsdtprod_flote-us_cancel
               dt_cancel = wa_zsdtprod_flote-dt_cancel
               hr_cancel = wa_zsdtprod_flote-hr_cancel
        WHERE docnum EQ wa_check_zsdtprod_flote-docnum
          AND itmnum EQ wa_check_zsdtprod_flote-itmnum
          AND werks  EQ wa_check_zsdtprod_flote-werks
          AND seq    EQ wa_check_zsdtprod_flote-seq.
* Efetiva ou retorno manutenção de tabela transparente
        PERFORM zf_commit_or_rollback_tbl USING sy-subrc.
        CLEAR wa_zsdtprod_flote.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form inicializa_variaiveis
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM inicializa_variaiveis .

*** CFOP Fins exportação
  SELECT *
    FROM zsdt0353
    INTO TABLE @DATA(lt_zsdt0353)
    WHERE excluido EQ @space.
  IF sy-subrc IS INITIAL.
    LOOP AT lt_zsdt0353 ASSIGNING FIELD-SYMBOL(<fs_zsdt0353>).

      r_cfop_e_fins-sign   = 'I'.
      r_cfop_e_fins-option = 'EQ'.
      r_cfop_e_fins-low    = <fs_zsdt0353>-cfop.
      APPEND r_cfop_e_fins.
    ENDLOOP.

  ENDIF.

  "-----------------------------------------------------------
*** CFOP Comercialização
  SELECT *
    FROM zsdt0352
    INTO TABLE @DATA(lt_zsdt0352)
    WHERE excluido EQ @space.
  IF sy-subrc IS INITIAL.
    LOOP AT lt_zsdt0352 ASSIGNING FIELD-SYMBOL(<fs_zsdt0352>).
      r_cfop_e_comerc-sign   = 'I'.
      r_cfop_e_comerc-option = 'EQ'.
      r_cfop_e_comerc-low    = <fs_zsdt0352>-cfop.
      APPEND r_cfop_e_comerc.
    ENDLOOP.
  ENDIF.

  APPEND LINES OF r_cfop_e_fins TO r_cfop_todos.
  APPEND LINES OF r_cfop_e_comerc TO r_cfop_todos.

  "------------------------------------------------------------------
*** Grupo de materiais
  SELECT *
    FROM zsdt0356
    INTO TABLE @DATA(lt_zsdt0356)
    WHERE excluido EQ @space.
  IF sy-subrc IS INITIAL.
    LOOP AT lt_zsdt0356 ASSIGNING FIELD-SYMBOL(<fs_zsdt0356>).
      r_matkl-sign   = 'I'.
      r_matkl-option = 'EQ'.
      r_matkl-low    = <fs_zsdt0356>-matkl.
      APPEND r_matkl.
    ENDLOOP.
  ENDIF.

  "------------------------------------------------------------------
*** CFOP Formação de lote
  SELECT *
    FROM zsdt0354
    INTO TABLE @DATA(lt_zsdt0354)
    WHERE excluido EQ @space.
  IF sy-subrc IS INITIAL.
    LOOP AT lt_zsdt0354 ASSIGNING FIELD-SYMBOL(<fs_zsdt0354>).
      r_cfop_form_lote-sign   = 'I'.
      r_cfop_form_lote-option = 'EQ'.
      r_cfop_form_lote-low    = <fs_zsdt0354>-cfop.
      APPEND r_cfop_form_lote.
    ENDLOOP.
  ENDIF.

*** CFOP Entrada por Transferência
  SELECT mandt mandt cfop
    FROM zsdt0362
    INTO TABLE gr_cfop_ent_trns
  WHERE excluido EQ space.

  IF sy-subrc IS INITIAL.
    r_cfop_form_lote-sign   = 'I'.
    r_cfop_form_lote-option = 'EQ'.

    MODIFY gr_cfop_ent_trns FROM r_cfop_form_lote TRANSPORTING sign option WHERE sign   EQ sy-mandt(1)
                                                                             AND option EQ sy-mandt(2).

    IF NOT gr_cfop_ent_trns IS INITIAL.
      APPEND LINES OF gr_cfop_ent_trns TO r_cfop_form_lote.
      APPEND LINES OF gr_cfop_ent_trns TO r_cfop_todos. " BUG SOLTO 189734 - SMC - 04-09-2025
      CLEAR r_cfop_form_lote.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form selec_notas_retorno_form_lote
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selec_notas_retorno_form_lote.

  DATA: t_cfop_ret_form_lote TYPE TABLE OF rgsb4,
        w_cfop_ret_form_lote TYPE rgsb4,
        r_cfop_retorno       TYPE RANGE OF j_1bnflin-cfop,
        ti_notas_retorno     TYPE TABLE OF ty_nf_entradas,
        lt_ret_flote         TYPE TABLE OF zsdtflote_flote.

  TABLES: zsdtflote_flote, *zsdtflote_flote.

  DATA: lv_exit       TYPE c,
        lv_exit2      TYPE c,
        lv_exit_do    TYPE c,
        lv_exit_do2   TYPE c,
        lv_export     TYPE c,
        lv_subtraendo TYPE j_1bnetqty.

*** CFOP Retorno de formação de lote
  SELECT *
    FROM zsdt0355
    INTO TABLE @DATA(lt_zsdt0355)
    WHERE excluido EQ @space.
  IF sy-subrc IS INITIAL.
    LOOP AT lt_zsdt0355 ASSIGNING FIELD-SYMBOL(<fs_zsdt0355>).
      APPEND INITIAL LINE TO r_cfop_retorno ASSIGNING FIELD-SYMBOL(<fs_cfop_retorno>).
      <fs_cfop_retorno>-sign   = 'I'.
      <fs_cfop_retorno>-option = 'EQ'.
      <fs_cfop_retorno>-low    = <fs_zsdt0355>-cfop.
    ENDLOOP.
  ENDIF.

  SELECT li~docnum li~itmnum li~matnr li~cfop li~menge li~werks li~matkl dc~docdat dc~cancel
         FROM j_1bnfdoc AS dc INNER JOIN j_1bnflin AS li
           ON li~docnum EQ dc~docnum
         INNER JOIN j_1bnfe_active AS ac
           ON dc~docnum EQ ac~docnum
         INTO TABLE ti_notas_retorno
         WHERE dc~pstdat IN s_docdat
         AND   dc~direct EQ '1'
         AND   dc~model  EQ '55'
         AND   dc~nftype EQ 'ZV'
         AND   dc~doctyp EQ '6'
         AND   dc~branch IN s_branch
         AND   dc~cancel NE 'X'
         AND   li~cfop   IN r_cfop_retorno
         AND   li~matkl  IN r_matkl
         AND   ac~docsta EQ '1'
         AND   ac~scssta NE '2'.

  CHECK ti_notas_retorno[] IS NOT INITIAL.

  DATA(lt_notas_retorno) = ti_notas_retorno.
  DATA: lr_finalidades TYPE RANGE OF zsdt0359-finalidade.

  SORT lt_notas_retorno BY docnum.
  DELETE ADJACENT DUPLICATES FROM lt_notas_retorno COMPARING docnum.

*** Finalidades
  SELECT * FROM zsdt0359 INTO TABLE @DATA(lt_zsdt0359) WHERE excluido EQ @space.

  IF sy-subrc IS INITIAL.
    lr_finalidades = VALUE #( FOR ls_zsdt0359 IN lt_zsdt0359 ( sign = 'I' option = 'EQ' low = ls_zsdt0359-finalidade ) ).

  ENDIF.
* Finalidades RFL - Fila NF Retorno de Recusa.
  DATA rl_final_ret_r TYPE RANGE OF zfin_export.

  SELECT * FROM zsdt0364 INTO TABLE @DATA(tl_zsdt0364) WHERE excluido EQ @space.

  IF sy-subrc IS INITIAL.
    rl_final_ret_r = VALUE #( FOR el_zsdt0364 IN tl_zsdt0364 ( sign = 'I' option = 'EQ' low = el_zsdt0364-finalidade ) ).

    IF NOT rl_final_ret_r IS INITIAL.
      APPEND LINES OF rl_final_ret_r TO lr_finalidades.

    ENDIF.

  ENDIF.

  SELECT * FROM  zsdt_export
    INTO TABLE @DATA(lt_export)
    FOR ALL ENTRIES IN @lt_notas_retorno
  WHERE docnum = @lt_notas_retorno-docnum
    AND finalidade  IN @lr_finalidades
    AND process_1x1 EQ @space.

  CHECK lt_export[] IS NOT INITIAL.
  SORT lt_export BY docnum.

  LOOP AT lt_export INTO DATA(el_export).

    SELECT * FROM  zsdt_retlote INTO TABLE @DATA(lt_retlote) WHERE docnum_ret EQ @el_export-docnum.

    CHECK lt_retlote[] IS NOT INITIAL.

    SORT lt_retlote BY docnum.

    DATA(lt_retlote_aux) = lt_retlote.
    SORT lt_retlote_aux BY docnum.
    DELETE ADJACENT DUPLICATES FROM lt_retlote_aux COMPARING docnum.

    SELECT a~docnum,b~itmnum,b~werks,c~centro_real,a~docdat,b~matnr,b~menge, b~ownpro
      FROM j_1bnfdoc AS a
      INNER JOIN j_1bnflin AS b
        ON a~docnum EQ b~docnum
      INNER JOIN zsdt_depara_cen AS c
        ON c~centrov_1 EQ b~werks
      INTO TABLE @DATA(lt_retorno)
      FOR ALL ENTRIES IN @lt_retlote_aux
    WHERE a~docnum EQ @lt_retlote_aux-docnum.

    CHECK lt_retorno[] IS NOT INITIAL.

    SELECT docnum_flote, docnum_eprod, id_vinc, docnum_ref, qtd_vinc, vinc_virtual
      FROM zsdtvinc_p_flote
      INTO TABLE @DATA(lt_vinclote)
      FOR ALL ENTRIES IN @lt_retlote_aux
    WHERE docnum_flote EQ @lt_retlote_aux-docnum
      AND cancel       EQ @space.

    IF sy-subrc IS INITIAL.
      SORT lt_vinclote BY docnum_flote docnum_eprod.

      DATA(lt_vinc) = lt_vinclote.
      SORT lt_vinc BY docnum_eprod.
      DELETE ADJACENT DUPLICATES FROM lt_vinc COMPARING docnum_eprod.

      SELECT docnum,compra_fim_es,saldo_vinc,saldo_disponivel,data_emissao
        FROM zsdtprod_flote
        INTO TABLE @DATA(lt_prod_flote)
        FOR ALL ENTRIES IN @lt_vinc
      WHERE docnum EQ @lt_vinc-docnum_eprod
        AND cancel EQ @space.

      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING lt_prod_flote[] TO ti_prod_flote[].
        SORT ti_prod_flote BY docnum.

      ENDIF.

      SELECT docnum,compra_fim_es,saldo_vinc,saldo_disponivel,data_emissao
        FROM zsdtflote_flote
        INTO TABLE @DATA(lt_flote_flote)
        FOR ALL ENTRIES IN @lt_vinc
      WHERE docnum EQ @lt_vinc-docnum_eprod
        AND cancel EQ @space.

      IF sy-subrc IS INITIAL.
        SORT lt_flote_flote BY docnum.

      ENDIF.

      DELETE lt_vinc WHERE vinc_virtual IS INITIAL.

      IF NOT lt_vinc[] IS INITIAL.
        SELECT docnum compra_fim_es saldo_vinc saldo_disponivel data_emissao
          FROM zsdtflote_flote
          APPENDING TABLE lt_flote_flote
          FOR ALL ENTRIES IN lt_vinc
        WHERE docnum EQ lt_vinc-docnum_flote
          AND cancel EQ space.

        IF sy-subrc IS INITIAL.
          SORT lt_flote_flote BY docnum.

        ENDIF.

      ENDIF.

      SELECT * FROM zsdtfl_flote_ref
        INTO TABLE @DATA(tl_fl_flote_ref)
        FOR ALL ENTRIES IN @lt_vinclote
      WHERE docnum_flote EQ @lt_vinclote-docnum_ref
        AND docnum_eprod EQ @lt_vinclote-docnum_eprod
        AND cancel       EQ @space.

      IF sy-subrc IS INITIAL.
        SORT tl_fl_flote_ref BY docnum_flote docnum_eprod.

      ENDIF.

    ENDIF.
* Prepara dados para processamento de Finalidade de cada regsitro da NF Retorno.
    lt_vinc        = lt_vinclote.
    lt_retlote_aux = lt_retlote.
    SORT: lt_vinc          BY docnum_flote docnum_eprod,
          lt_retlote       BY docnum docnum_ret,
          lt_retlote_aux   BY docnum docnum_ret,
          lt_prod_flote    BY data_emissao DESCENDING docnum DESCENDING,
          lt_flote_flote   BY data_emissao DESCENDING docnum DESCENDING.
* Cria as TIs de atualização.
    DATA(lt_prod_flote_up)   = lt_prod_flote.
    DATA(lt_flote_flote_up)  = lt_flote_flote.
    DATA(lt_flote_flote_up2) = lt_flote_flote_up.
    DATA(lt_vinc_up)         = lt_vinc.
    DATA(lt_export_up)       = lt_export.
* Limpa as TIs de atualização.
    CLEAR: lt_prod_flote_up, lt_flote_flote_up, lt_flote_flote_up2, lt_vinc_up, lt_export_up.

    LOOP AT lt_retorno ASSIGNING FIELD-SYMBOL(<fs_retorno>).
* Busca a Finalidade de cada regsitro da NF Retorno.
      LOOP AT lt_retlote INTO DATA(el_retlote) WHERE docnum = <fs_retorno>-docnum.
* Verifica se faz parte da Finalidade RFL - Fila NF Retorno de Recusa.
        IF el_export-finalidade IN rl_final_ret_r.

          LOOP AT lt_retlote_aux INTO DATA(el_retlote2) WHERE docnum     EQ el_retlote-docnum
                                                          AND docnum_ret EQ el_retlote-docnum_ret.
* Cálculo do Saldo Disponível e Saldo Vinculado.
            LOOP AT lt_vinc INTO DATA(el_vinclote) WHERE docnum_flote EQ el_retlote2-docnum.
              DATA(lv_tbx_vinc) = sy-tabix.

              DO.
* Validador de leitura das TIs LT_PROD_FLOTE e LT_FLOTE_FLOTE no laço do DO...ENDDO.
                CLEAR lv_exit_do2.
* Busca Notas de Produtor para Vincular.
                READ TABLE lt_prod_flote INTO DATA(el_prod_flote) WITH KEY docnum = el_vinclote-docnum_eprod.
* Valida se o registro da TI Vínculo é de exckuisividade da Tabela ZSDTFLOTE_FLOTE.
                IF sy-subrc                 IS INITIAL      AND
                   el_vinclote-docnum_ref   EQ '0000000000' AND
                   el_vinclote-vinc_virtual EQ abap_off.
                  CLEAR sy-subrc.

                ELSE.
                  sy-subrc = 4.

                ENDIF.

                IF sy-subrc IS INITIAL.
                  lv_exit_do2    = abap_on.
                  DATA(lv_tabix) = sy-tabix.
* Definição do subtraendo conforme quantidade envolvidas.
                  PERFORM zf_define_subtract_calc TABLES   rl_final_ret_r
                                                  USING    <fs_retorno>-menge
                                                           el_retlote2-quant_vinc
                                                           el_prod_flote-saldo_vinc
                                                           el_vinclote-qtd_vinc
                                                           el_export-finalidade
                                                  CHANGING lv_subtraendo.
* Calcular valores de saldos da NF Recusada.
                  PERFORM zf_calc_saldos_nf_recusada TABLES lt_vinc_up
                                                            lt_vinc
                                                            lt_prod_flote
                                                            lt_prod_flote_up
                                                            tl_fl_flote_ref
                                                            rl_final_ret_r
                                                      USING lv_tbx_vinc
                                                            lv_tabix
                                                            lv_subtraendo
                                                            el_export-finalidade
                                                            el_vinclote
                                                            el_prod_flote
                                                   CHANGING <fs_retorno>-menge
                                                            el_retlote2-quant_vinc
                                                            el_vinclote-qtd_vinc
                                                            el_prod_flote-saldo_vinc
                                                            el_prod_flote-saldo_disponivel
                                                            lv_exit
                                                            lv_exit2
                                                            lv_exit_do
                                                            lv_export.
* Verifica se é para sair do Laço DO...ENDDO.
                  IF NOT lv_exit_do IS INITIAL.
                    EXIT.

                  ENDIF.

                  IF el_vinclote-qtd_vinc IS INITIAL.
                    CLEAR: el_prod_flote, el_vinclote.
                    EXIT.

                  ENDIF.

                  IF el_retlote2-quant_vinc IS INITIAL.
                    CLEAR: el_prod_flote, el_vinclote.
                    EXIT.

                  ENDIF.

                ELSE.
* Busca Notas de formação de lote Recusada para Vincular.
                  READ TABLE lt_flote_flote INTO DATA(el_flote_flote) WITH KEY docnum = el_vinclote-docnum_eprod.

                  IF sy-subrc IS INITIAL.
                    lv_exit_do2 = abap_on.
                    lv_tabix    = sy-tabix.
* Definição do subtraendo conforme quantidade envolvidas.
                    PERFORM zf_define_subtract_calc TABLES   rl_final_ret_r
                                                    USING    <fs_retorno>-menge
                                                             el_retlote2-quant_vinc
                                                             el_flote_flote-saldo_vinc
                                                             el_vinclote-qtd_vinc
                                                             el_export-finalidade
                                                    CHANGING lv_subtraendo.

* Calcular valores de saldos da NF Recusada.
                    PERFORM zf_calc_saldos_nf_recusada TABLES lt_vinc_up
                                                              lt_vinc
                                                              lt_flote_flote
                                                              lt_flote_flote_up
                                                              tl_fl_flote_ref
                                                              rl_final_ret_r
                                                        USING lv_tbx_vinc
                                                              lv_tabix
                                                              lv_subtraendo
                                                              el_export-finalidade
                                                              el_vinclote
                                                              el_flote_flote
                                                     CHANGING <fs_retorno>-menge
                                                              el_retlote2-quant_vinc
                                                              el_vinclote-qtd_vinc
                                                              el_flote_flote-saldo_vinc
                                                              el_flote_flote-saldo_disponivel
                                                              lv_exit
                                                              lv_exit2
                                                              lv_exit_do
                                                              lv_export.
* Verifica se é para sair do Laço DO...ENDDO.
                    IF NOT lv_exit_do IS INITIAL.
                      EXIT.

                    ENDIF.

                  ENDIF.

                  IF el_vinclote-qtd_vinc IS INITIAL.
                    CLEAR: el_flote_flote, el_vinclote.
                    EXIT.

                  ENDIF.

                  IF el_retlote2-quant_vinc IS INITIAL.
                    CLEAR: el_flote_flote, el_vinclote.
                    EXIT.

                  ENDIF.
* Verifica se não houve nenhum processamento tanto na parte de formação de lote com a NF do produtor
* (ZSDTPROD_FLOTE) quanto do lote do retorno da formação de lote (ZSDTFLOTE_FLOTE) para sair do laço
* DO...ENDDO.
                  IF lv_exit_do2 IS INITIAL.
                    EXIT.

                  ENDIF.

                ENDIF.

              ENDDO.

              IF NOT lv_exit IS INITIAL.
                CLEAR lv_exit.
                EXIT.

              ENDIF.

            ENDLOOP.

            IF NOT lv_exit2 IS INITIAL.
              CLEAR lv_exit2.
              EXIT.

            ENDIF.

          ENDLOOP.

          IF NOT lv_export IS INITIAL.
* Prepara a atualização da Tabela Export.
            PERFORM zf_update_tab_export TABLES lt_export_up
                                          USING el_export.
            CLEAR lv_export.

          ENDIF.

          CLEAR: el_vinclote.

        ELSE.
* Verifica se a Nota de Retorno é de Produção Própria.
          IF NOT <fs_retorno>-ownpro IS INITIAL.
            sy-subrc = 4.
* Verifica se há registro na Tabela de Vínculo para processar normalmente
            LOOP AT lt_vinclote INTO el_vinclote WHERE docnum_flote EQ el_retlote-docnum
                                                   AND docnum_ref   IS INITIAL.
              CLEAR sy-subrc.
* Verifica se há vínculo para a Nota de Retorno que é rodução Própria.
              DATA(vl_vinc_ownpro) = abap_on.
              EXIT.

            ENDLOOP.

            IF NOT sy-subrc IS INITIAL.
* Cria registro na Tabela Notas de formação de lote Recusada para Vincular.
              CLEAR zsdtflote_flote.
              MOVE: <fs_retorno>-docnum      TO zsdtflote_flote-docnum,
                    <fs_retorno>-itmnum      TO zsdtflote_flote-itmnum,
                    <fs_retorno>-werks       TO zsdtflote_flote-werks,
                    <fs_retorno>-centro_real TO zsdtflote_flote-werks_real,
                    <fs_retorno>-docdat      TO zsdtflote_flote-data_emissao,
                    <fs_retorno>-matnr       TO zsdtflote_flote-material,
                    <fs_retorno>-menge       TO zsdtflote_flote-qtd_nf,
                    0                        TO zsdtflote_flote-saldo_vinc,
                    <fs_retorno>-menge       TO zsdtflote_flote-saldo_disponivel,
                    abap_off                 TO zsdtflote_flote-saldo_nao_disponivel,
                    abap_off                 TO zsdtflote_flote-compra_fim_es.
* Marcar registro para criação de linha nova.
              PERFORM zf_set_line_to_new USING zsdtflote_flote.
* Gera o número sequencial do campo SEQ das tabelas PROD_FLOTE, FLOTE_FLOTE, FL_FLOTE_REF.
              PERFORM zf_number_get_next USING    '01'
                                                  'ZSEQFLOTE'
                                         CHANGING zsdtflote_flote-seq.
* Busca o campo EUDR da Nota Fiscal de entrada.
              zcl_eudr_utils=>check_doc_fiscal_eudr( EXPORTING
                                                       i_docnum = zsdtflote_flote-docnum
                                                     RECEIVING
                                                       r_eudr   = zsdtflote_flote-eudr
                                                    ).

              APPEND zsdtflote_flote TO lt_ret_flote.
              CLEAR: zsdtflote_flote.
* Prepara a atualização da Tabela Export.
              PERFORM zf_update_tab_export TABLES lt_export_up
                                            USING el_export.

            ENDIF.

          ELSE.
            CLEAR vl_vinc_ownpro.

          ENDIF.
* Processamento da Saldo Vínculo do Lote para atualização da tabela.
          CLEAR: el_vinclote, zsdtflote_flote.
          LOOP AT lt_vinclote INTO el_vinclote WHERE docnum_flote EQ el_retlote-docnum
                                                 AND docnum_ref   IS INITIAL.
            lv_tbx_vinc = sy-tabix.
            CLEAR zsdtflote_flote.

            IF el_vinclote-vinc_virtual IS INITIAL.
              zsdtflote_flote-docnum = el_vinclote-docnum_flote.

            ELSE.
              zsdtflote_flote-docnum = el_vinclote-docnum_eprod.

            ENDIF.
* Verifica se o registro em questão existe na FLOTE_FLOTE e se ele já foi lido.
            IF *zsdtflote_flote-docnum NE zsdtflote_flote-docnum.
*** Verifica se existe na ZSDTFLOTE_FLOTE.
              CLEAR el_flote_flote.
              READ TABLE lt_flote_flote INTO el_flote_flote WITH KEY docnum = zsdtflote_flote-docnum.

              IF sy-subrc IS INITIAL.
                lv_tabix = sy-tabix.
* Lê a tabela Notas de formação de lote Recusada para Vincular
                PERFORM zf_read_tab_zsdtflote_flote USING    zsdtflote_flote-docnum
                                                             zsdtflote_flote
                                                    CHANGING sy-subrc.

                IF sy-subrc IS INITIAL.
                  DATA(lv_sel_flot) = abap_on.
                  *zsdtflote_flote = zsdtflote_flote.

                ENDIF.

              ELSE.
* Lê a tabela Notas de formação de lote Recusada para Vincular
                PERFORM zf_read_tab_zsdtflote_flote USING    zsdtflote_flote-docnum
                                                             zsdtflote_flote
                                                    CHANGING sy-subrc.

                IF sy-subrc IS INITIAL.
                  lv_sel_flot = abap_on.
                  CLEAR lv_tabix.
                  *zsdtflote_flote = zsdtflote_flote.

                ELSE.
* Registro novo de quebra de vínculo.
                  zsdtflote_flote-itmnum       = <fs_retorno>-itmnum.
                  zsdtflote_flote-werks        = <fs_retorno>-werks.
                  zsdtflote_flote-werks_real   = <fs_retorno>-centro_real.
                  zsdtflote_flote-data_emissao = <fs_retorno>-docdat.
                  zsdtflote_flote-material     = <fs_retorno>-matnr.

                  IF el_retlote-quant_vinc LE el_vinclote-qtd_vinc.
                    zsdtflote_flote-saldo_disponivel = el_retlote-quant_vinc.
                    zsdtflote_flote-qtd_nf           = el_retlote-quant_vinc.
                    el_vinclote-qtd_vinc             = el_retlote-quant_vinc.

                  ELSE.
                    zsdtflote_flote-saldo_disponivel = el_vinclote-qtd_vinc.
                    zsdtflote_flote-qtd_nf           = el_vinclote-qtd_vinc.

                  ENDIF.

                ENDIF.

              ENDIF.

            ELSE.
              zsdtflote_flote = *zsdtflote_flote.
              MOVE-CORRESPONDING el_flote_flote TO zsdtflote_flote.

            ENDIF.

            IF NOT lv_sel_flot IS INITIAL.
              IF el_flote_flote IS INITIAL.
                MOVE-CORRESPONDING zsdtflote_flote TO el_flote_flote.
                APPEND el_flote_flote TO lt_flote_flote.
                lv_tabix = sy-tabix.
                CLEAR el_flote_flote.

              ENDIF.
* Marcar registro para cancelamento.
              PERFORM zf_set_line_to_cancel USING zsdtflote_flote.

              APPEND zsdtflote_flote TO lt_ret_flote.
* Veifica se o Saldo está disponível.
              IF zsdtflote_flote-saldo_nao_disponivel IS INITIAL.
                IF el_retlote-quant_vinc LE el_vinclote-qtd_vinc.
                  zsdtflote_flote-saldo_disponivel = zsdtflote_flote-saldo_disponivel + el_retlote-quant_vinc.

                ELSE.
                  zsdtflote_flote-saldo_disponivel = zsdtflote_flote-saldo_disponivel + el_vinclote-qtd_vinc.

                ENDIF.

              ENDIF.

              CLEAR lv_sel_flot.

            ENDIF.
* Marcar registro para criação de linha nova.
            PERFORM zf_set_line_to_new USING zsdtflote_flote.

            READ TABLE lt_ret_flote TRANSPORTING NO FIELDS WITH KEY docnum = zsdtflote_flote-docnum
                                                                    itmnum = zsdtflote_flote-itmnum
                                                                    cancel = abap_off.
            IF NOT sy-subrc IS INITIAL.
* Gera o número sequencial do campo SEQ das tabelas PROD_FLOTE, FLOTE_FLOTE, FL_FLOTE_REF.
              PERFORM zf_number_get_next USING    '01'
                                                  'ZSEQFLOTE'
                                         CHANGING zsdtflote_flote-seq.

            ENDIF.

            IF el_flote_flote IS INITIAL.
              MOVE-CORRESPONDING zsdtflote_flote TO el_flote_flote.
* Veifica se o Saldo está disponível.
              IF NOT zsdtflote_flote-saldo_nao_disponivel IS INITIAL.
                IF el_retlote-quant_vinc LE el_vinclote-qtd_vinc.
                  el_flote_flote-saldo_disponivel = el_retlote-quant_vinc.

                ELSE.
                  el_flote_flote-saldo_disponivel = el_vinclote-qtd_vinc.

                ENDIF.

              ENDIF.

            ENDIF.
* Definição do subtraendo conforme quantidade envolvidas.
            PERFORM zf_define_subtract_calc TABLES   rl_final_ret_r
                                            USING    <fs_retorno>-menge
                                                     el_retlote-quant_vinc
                                                     zsdtflote_flote-saldo_vinc
                                                     el_vinclote-qtd_vinc
                                                     el_export-finalidade
                                            CHANGING lv_subtraendo.
* Calcular valores de saldos da NF Recusada.
            PERFORM zf_calc_saldos_nf_recusada TABLES lt_vinc_up
                                                      lt_vinc
                                                      lt_flote_flote
                                                      lt_flote_flote_up2
                                                      tl_fl_flote_ref
                                                      rl_final_ret_r
                                                USING lv_tbx_vinc
                                                      lv_tabix
                                                      lv_subtraendo
                                                      el_export-finalidade
                                                      el_vinclote
                                                      el_flote_flote
                                             CHANGING <fs_retorno>-menge
                                                      el_retlote-quant_vinc
                                                      el_vinclote-qtd_vinc
                                                      el_flote_flote-saldo_vinc
                                                      el_flote_flote-saldo_disponivel
                                                      lv_exit
                                                      lv_exit2
                                                      lv_exit_do
                                                      lv_export.

* Veifica se o Saldo está disponível.
            IF zsdtflote_flote-saldo_nao_disponivel IS INITIAL.
* Verifica se não é um Vínculo Virtual.
              IF el_vinclote-vinc_virtual IS INITIAL.
                zsdtflote_flote-saldo_vinc = el_vinclote-qtd_vinc.

              ELSE.
                zsdtflote_flote-saldo_vinc = el_flote_flote-saldo_vinc.

              ENDIF.

            ELSE.
* Comportamento de novo registro. Registro existe na FLOTE_FLOTE e se ele já foi lido.
              zsdtflote_flote-saldo_vinc       = zsdtflote_flote-saldo_vinc       - lv_subtraendo.
              zsdtflote_flote-saldo_disponivel = zsdtflote_flote-saldo_disponivel + lv_subtraendo.

            ENDIF.

            READ TABLE ti_prod_flote INTO DATA(el_prod_flote2) WITH KEY docnum = el_vinclote-docnum_eprod BINARY SEARCH.

            IF sy-subrc IS INITIAL.
              IF el_prod_flote2-compra_fim_es IS NOT INITIAL.
                zsdtflote_flote-compra_fim_es = abap_true.

              ENDIF.

            ENDIF.

            READ TABLE lt_ret_flote INTO DATA(el_ret_flote) WITH KEY docnum = zsdtflote_flote-docnum
                                                                     itmnum = zsdtflote_flote-itmnum
                                                                     cancel = abap_off.

            IF sy-subrc IS INITIAL.
* Verifica se o Saldo está disponível.
              IF zsdtflote_flote-saldo_nao_disponivel IS INITIAL.
* Verifica se o registro em questão existe na FLOTE_FLOTE e se ele já foi lido.
                IF *zsdtflote_flote-docnum EQ zsdtflote_flote-docnum.
                  el_ret_flote-qtd_nf           = el_ret_flote-qtd_nf           + lv_subtraendo.
                  el_ret_flote-saldo_disponivel = el_ret_flote-saldo_disponivel + lv_subtraendo.

                ELSE.
                  el_ret_flote-qtd_nf           = el_ret_flote-qtd_nf           + zsdtflote_flote-qtd_nf.
                  el_ret_flote-saldo_disponivel = el_ret_flote-saldo_disponivel + zsdtflote_flote-saldo_disponivel.

                ENDIF.

                IF el_ret_flote-qtd_nf NE el_ret_flote-saldo_disponivel.
                  el_ret_flote-saldo_vinc = el_ret_flote-saldo_vinc + zsdtflote_flote-saldo_vinc.

                ENDIF.

                MODIFY lt_ret_flote FROM el_ret_flote INDEX sy-tabix TRANSPORTING qtd_nf saldo_vinc saldo_disponivel.
                CLEAR el_ret_flote.

              ELSE.
* Verifica se há saldo disponível.
                IF NOT zsdtflote_flote-saldo_disponivel IS INITIAL.
                  CLEAR: zsdtflote_flote-saldo_nao_disponivel.

                ENDIF.

                MODIFY lt_ret_flote FROM zsdtflote_flote INDEX sy-tabix TRANSPORTING qtd_nf saldo_vinc saldo_disponivel saldo_nao_disponivel.
                MOVE-CORRESPONDING zsdtflote_flote TO el_flote_flote.
                CLEAR el_ret_flote.

              ENDIF.

            ELSE.
* Verifica se há saldo disponível.
              IF NOT zsdtflote_flote-saldo_disponivel IS INITIAL.
                CLEAR: zsdtflote_flote-saldo_nao_disponivel.

              ENDIF.
* Verifica se não é um vinculo virtual
              IF el_vinclote-vinc_virtual IS INITIAL.
                READ TABLE lt_ret_flote TRANSPORTING NO FIELDS WITH KEY docnum = zsdtflote_flote-docnum
                                                                        itmnum = zsdtflote_flote-itmnum
                                                                        cancel = abap_on.

                IF sy-subrc IS INITIAL.
                  zsdtflote_flote-qtd_nf = zsdtflote_flote-qtd_nf + lv_subtraendo.

                ENDIF.

              ENDIF.

              APPEND zsdtflote_flote TO lt_ret_flote.
              MOVE-CORRESPONDING zsdtflote_flote TO el_flote_flote.

            ENDIF.

            IF NOT lv_export IS INITIAL.
* Prepara a atualização da Tabela Export.
              PERFORM zf_update_tab_export TABLES lt_export_up
                                            USING el_export.
              CLEAR lv_export.

            ENDIF.

            IF NOT lv_exit IS INITIAL.
              CLEAR lv_exit.
              EXIT.

            ENDIF.

          ENDLOOP.

        ENDIF.

        IF NOT lv_exit2 IS INITIAL.
          CLEAR lv_exit2.
          EXIT.

        ENDIF.

      ENDLOOP.
* Verifica se ainda tem saldo de do retorno da nota
* Se a Nota de Retorno é de Produção Própria.
* Se há vínculo para a Nota de Retorno que é rodução Própria.
* E se não faz parte da Finalidade RFL - Fila NF Retorno de Recusa.
      IF     el_retlote-quant_vinc GT 0                  AND
         NOT <fs_retorno>-ownpro   IS INITIAL            AND
         NOT vl_vinc_ownpro        IS INITIAL            AND
             el_export-finalidade  NOT IN rl_final_ret_r.
* Cria registro na Tabela Notas de formação de lote Recusada para Vincular.
        CLEAR zsdtflote_flote.
        MOVE: <fs_retorno>-docnum      TO zsdtflote_flote-docnum,
              <fs_retorno>-itmnum      TO zsdtflote_flote-itmnum,
              <fs_retorno>-werks       TO zsdtflote_flote-werks,
              <fs_retorno>-centro_real TO zsdtflote_flote-werks_real,
              <fs_retorno>-docdat      TO zsdtflote_flote-data_emissao,
              <fs_retorno>-matnr       TO zsdtflote_flote-material,
              el_retlote-quant_vinc    TO zsdtflote_flote-qtd_nf,
              0                        TO zsdtflote_flote-saldo_vinc,
              el_retlote-quant_vinc    TO zsdtflote_flote-saldo_disponivel,
              abap_off                 TO zsdtflote_flote-saldo_nao_disponivel,
              abap_off                 TO zsdtflote_flote-compra_fim_es.
* Marcar registro para criação de linha nova.
        PERFORM zf_set_line_to_new USING zsdtflote_flote.
* Gera o número sequencial do campo SEQ das tabelas PROD_FLOTE, FLOTE_FLOTE, FL_FLOTE_REF.
        PERFORM zf_number_get_next USING    '01'
                                            'ZSEQFLOTE'
                                   CHANGING zsdtflote_flote-seq.
* Busca o campo EUDR da Nota Fiscal de entrada.
        zcl_eudr_utils=>check_doc_fiscal_eudr( EXPORTING
                                                 i_docnum = zsdtflote_flote-docnum
                                               RECEIVING
                                                 r_eudr   = zsdtflote_flote-eudr
                                              ).

        APPEND zsdtflote_flote TO lt_ret_flote.
        CLEAR: zsdtflote_flote, vl_vinc_ownpro.
* Prepara a atualização da Tabela Export.
        PERFORM zf_update_tab_export TABLES lt_export_up
                                      USING el_export.

      ENDIF.

    ENDLOOP.

    IF lt_ret_flote IS NOT INITIAL.
      MODIFY zsdtflote_flote FROM TABLE lt_ret_flote.
* Efetiva ou retorno manutenção de tabela transparente.
      PERFORM zf_commit_or_rollback_tbl USING sy-subrc.

    ENDIF.

    IF NOT lt_export_up[] IS INITIAL.
      MODIFY zsdt_export FROM TABLE lt_export_up.
* Efetiva ou retorno manutenção de tabela transparente.
      PERFORM zf_commit_or_rollback_tbl USING sy-subrc.

    ENDIF.

    DATA: tl_prod_flote TYPE TABLE OF zsdtprod_flote WITH HEADER LINE.
* Atualizar dados na tabela de Notas de Produtor para Vincular.
    IF NOT lt_prod_flote_up[] IS INITIAL.
* Busca dados da Tabela de Notas de Produtor para atualização da tabela quando a SALDO_VINC for modificado.
      SELECT * FROM zsdtprod_flote
        INTO TABLE tl_prod_flote
        FOR ALL ENTRIES IN lt_prod_flote_up
      WHERE docnum EQ lt_prod_flote_up-docnum
        AND cancel EQ space.

      IF sy-subrc IS INITIAL.
        CLEAR el_prod_flote.
        LOOP AT lt_prod_flote_up INTO el_prod_flote.
          READ TABLE tl_prod_flote WITH KEY docnum = el_prod_flote-docnum.

          IF sy-subrc IS INITIAL.
* Marcar registro para cancelamento.
            PERFORM zf_set_line_to_cancel USING tl_prod_flote.

            MODIFY zsdtprod_flote FROM tl_prod_flote.

            IF sy-subrc IS INITIAL.
              MOVE: el_prod_flote-saldo_vinc       TO tl_prod_flote-saldo_vinc,
                    el_prod_flote-saldo_disponivel TO tl_prod_flote-saldo_disponivel,
                    abap_off                       TO tl_prod_flote-saldo_nao_disponivel.
* Marcar registro para criação de linha nova.
              PERFORM zf_set_line_to_new USING tl_prod_flote.
* Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE, ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF.
              PERFORM zf_number_get_next USING    '01'
                                                  'ZSEQPROD'
                                         CHANGING tl_prod_flote-seq.

              MODIFY zsdtprod_flote FROM tl_prod_flote.
* Efetiva ou retorno manutenção de tabela transparente.
              PERFORM zf_commit_or_rollback_tbl USING sy-subrc.

            ELSE.
              ROLLBACK WORK.

            ENDIF.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    DATA: tl_flote_flote TYPE TABLE OF zsdtflote_flote WITH HEADER LINE.
* Atualizar dados na tabela de Notas de formação de lote Recusada para Vincular
    IF NOT lt_flote_flote_up[] IS INITIAL.
* Busca dados da Tabela de formação de lote para atualização da tabela quando a SALDO_VINC for modificado.
      SELECT * FROM zsdtflote_flote
        INTO TABLE tl_flote_flote
        FOR ALL ENTRIES IN lt_flote_flote_up
      WHERE docnum EQ lt_flote_flote_up-docnum
        AND cancel EQ space.

      IF sy-subrc IS INITIAL.
        CLEAR el_flote_flote.
        LOOP AT lt_flote_flote_up INTO el_flote_flote.
          READ TABLE tl_flote_flote WITH KEY docnum = el_flote_flote-docnum.

          IF sy-subrc IS INITIAL.
* Marcar registro para cancelamento.
            PERFORM zf_set_line_to_cancel USING tl_flote_flote.

            MODIFY zsdtflote_flote FROM tl_flote_flote.

            IF sy-subrc IS INITIAL.
              MOVE: el_flote_flote-saldo_vinc       TO tl_flote_flote-saldo_vinc,
                    el_flote_flote-saldo_disponivel TO tl_flote_flote-saldo_disponivel,
                    abap_off                        TO tl_flote_flote-saldo_nao_disponivel.
* Marcar registro para criação de linha nova.
              PERFORM zf_set_line_to_new USING tl_flote_flote.
* Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE, ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF.
              PERFORM zf_number_get_next USING    '01'
                                                  'ZSEQFLOTE'
                                         CHANGING tl_flote_flote-seq.

              MODIFY zsdtflote_flote FROM tl_flote_flote.
* Efetiva ou retorno manutenção de tabela transparente.
              PERFORM zf_commit_or_rollback_tbl USING sy-subrc.

            ELSE.
              ROLLBACK WORK.

            ENDIF.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    TABLES: zsdtfl_flote_ref.
    DATA: tl_vinc_p_flote TYPE TABLE OF zsdtvinc_p_flote WITH HEADER LINE.
* Atualizar dados na Tabela de vinculo entre a formação de lote.

    CHECK lt_vinc_up[] IS NOT INITIAL.
* Busca dados da Tabela de vículos para atualização da tabela quando a QTD_VINC não for "ZERO".
    SELECT * FROM zsdtvinc_p_flote
      INTO TABLE tl_vinc_p_flote
      FOR ALL ENTRIES IN lt_vinc_up
    WHERE docnum_flote EQ lt_vinc_up-docnum_flote
      AND docnum_eprod EQ lt_vinc_up-docnum_eprod
      AND id_vinc      EQ lt_vinc_up-id_vinc
      AND cancel       EQ space.

    CHECK tl_vinc_p_flote[] IS NOT INITIAL.

    CLEAR el_vinclote.
    LOOP AT lt_vinc_up INTO el_vinclote.
      CLEAR: el_retlote, el_export.
      READ TABLE lt_retlote INTO el_retlote WITH KEY docnum = el_vinclote-docnum_flote.
      READ TABLE lt_export  INTO el_export  WITH KEY docnum = el_retlote-docnum_ret.
      CLEAR tl_vinc_p_flote.
      READ TABLE tl_vinc_p_flote WITH KEY docnum_flote = el_vinclote-docnum_flote
                                          docnum_eprod = el_vinclote-docnum_eprod
                                          id_vinc      = el_vinclote-id_vinc
                                          docnum_ref   = el_vinclote-docnum_ref.

      IF sy-subrc IS INITIAL.
* Marcar registro para cancelamento.
        PERFORM zf_set_line_to_cancel USING tl_vinc_p_flote.

        MODIFY zsdtvinc_p_flote FROM tl_vinc_p_flote.

        IF sy-subrc IS INITIAL.
          DATA(lv_rc) = sy-subrc.
* Verifica se a Nota não é virtual e se não é referenecia de virtual.
          IF el_vinclote-vinc_virtual IS INITIAL AND
             el_vinclote-docnum_ref   IS INITIAL.

            READ TABLE lt_flote_flote_up2 INTO DATA(el_flote_flote_up2) WITH KEY docnum = el_vinclote-docnum_flote.

            IF sy-subrc IS INITIAL.
              DATA(vl_tabix) = sy-tabix.
* Verifica se na TI de FLOTE FLOTE de finalidade tipo O... há um registro em processamento cancelado.
              READ TABLE lt_ret_flote TRANSPORTING NO FIELDS WITH KEY docnum = el_vinclote-docnum_flote
                                                                      cancel = abap_on.

              IF NOT sy-subrc IS INITIAL.
                IF tl_vinc_p_flote-qtd_vinc LT el_flote_flote_up2-saldo_disponivel.
                  el_flote_flote_up2-saldo_disponivel = el_flote_flote_up2-saldo_disponivel - tl_vinc_p_flote-qtd_vinc.
                  MODIFY lt_flote_flote_up2 FROM el_flote_flote_up2 INDEX vl_tabix TRANSPORTING saldo_disponivel.

                ELSE.
                  el_vinclote-qtd_vinc = tl_vinc_p_flote-qtd_vinc - el_flote_flote_up2-saldo_disponivel.

                ENDIF.

                sy-subrc = lv_rc.

              ENDIF.

            ELSE.
              sy-subrc = lv_rc.

            ENDIF.

          ENDIF.

          DATA(lv_qtd_vinc_dif) = tl_vinc_p_flote-qtd_vinc - el_vinclote-qtd_vinc.

          IF el_vinclote-qtd_vinc IS NOT INITIAL.
            CLEAR zsdtfl_flote_ref.
            MOVE: el_vinclote-qtd_vinc TO tl_vinc_p_flote-qtd_vinc.
            ADD   1                    TO tl_vinc_p_flote-id_vinc.
* Marcar registro para criação de linha nova.
            PERFORM zf_set_line_to_new USING tl_vinc_p_flote.

            MODIFY zsdtvinc_p_flote FROM tl_vinc_p_flote.
            lv_rc = sy-subrc.

          ENDIF.
* Verifica se a Nota não é virtual.
          IF el_vinclote-vinc_virtual IS INITIAL.

            IF sy-subrc IS INITIAL.
              IF el_vinclote-docnum_ref IS INITIAL.
* Valida se o Registro processado é um FLOTE_FLOTE.
                PERFORM zf_reg_check_flote_flote TABLES    lt_flote_flote_up
                                                           lt_flote_flote_up2
                                                           rl_final_ret_r
                                                  USING    el_export-finalidade
                                                           el_vinclote-docnum_eprod
                                                           el_vinclote-docnum_flote
                                                  CHANGING sy-subrc.

              ELSE.
* Valida se o Registro processado é um FLOTE_FLOTE.
                PERFORM zf_reg_check_flote_flote TABLES    lt_flote_flote_up
                                                           lt_flote_flote_up2
                                                           rl_final_ret_r
                                                  USING    el_export-finalidade
                                                           el_vinclote-docnum_ref
                                                           el_vinclote-docnum_ref
                                                  CHANGING sy-subrc.

              ENDIF.

              IF sy-subrc IS INITIAL.
                CLEAR zsdtfl_flote_ref.
* Prepara e monta a atualização da tabela Tabela auxiliar para controle do Saldo do Liberado pelo Retono.
* Verifica se a Finalidade de Exportação é Finalidade de Retorno de Recusa.
                IF el_export-finalidade IN rl_final_ret_r.
* Lê a tabela Tabela auxiliar - Controle do Saldo do Liberado pelo Retorno.
                  PERFORM zf_read_tab_zsdtfl_flote_ref USING    el_vinclote-docnum_ref
                                                                el_vinclote-docnum_eprod
                                                       CHANGING zsdtfl_flote_ref
                                                                sy-subrc.

                ELSE.
                  IF el_vinclote-docnum_ref IS INITIAL.
* Lê a tabela Tabela auxiliar - Controle do Saldo do Liberado pelo Retorno.
                    PERFORM zf_read_tab_zsdtfl_flote_ref USING    el_vinclote-docnum_flote
                                                                  el_vinclote-docnum_eprod
                                                         CHANGING zsdtfl_flote_ref
                                                                  sy-subrc.

                  ELSE.
* Lê a tabela Tabela auxiliar - Controle do Saldo do Liberado pelo Retorno.
                    PERFORM zf_read_tab_zsdtfl_flote_ref USING    el_vinclote-docnum_ref
                                                                  el_vinclote-docnum_eprod
                                                         CHANGING zsdtfl_flote_ref
                                                                  sy-subrc.

                  ENDIF.

                ENDIF.

                IF sy-subrc IS INITIAL.
* Marcar registro para cancelamento.
                  PERFORM zf_set_line_to_cancel USING zsdtfl_flote_ref.

                  MODIFY zsdtfl_flote_ref.

                  IF sy-subrc IS INITIAL.
* Verifica se o Documento já existia na Tabela ZDDTFLOT_FLOT.
                    READ TABLE lt_flote_flote INTO el_flote_flote WITH KEY docnum = zsdtfl_flote_ref-docnum_flote.

                    IF sy-subrc IS INITIAL.
                      IF el_vinclote-docnum_ref IS INITIAL.
* Veifica se há Saldo disponível.
                        IF zsdtfl_flote_ref-saldo_disponivel IS INITIAL.
                          CLEAR: zsdtfl_flote_ref-qtd, zsdtfl_flote_ref-qtd_vinc, zsdtfl_flote_ref-saldo_disponivel.

                        ENDIF.

                        zsdtfl_flote_ref-qtd              = zsdtfl_flote_ref-qtd              + lv_qtd_vinc_dif.
                        zsdtfl_flote_ref-saldo_disponivel = zsdtfl_flote_ref-saldo_disponivel + lv_qtd_vinc_dif.

                      ELSE.
                        zsdtfl_flote_ref-qtd_vinc         = zsdtfl_flote_ref-qtd_vinc         - lv_qtd_vinc_dif.
                        zsdtfl_flote_ref-saldo_disponivel = zsdtfl_flote_ref-saldo_disponivel + lv_qtd_vinc_dif.

                      ENDIF.

                    ELSE.
                      zsdtfl_flote_ref-qtd_vinc         = zsdtfl_flote_ref-qtd_vinc         - lv_qtd_vinc_dif.
                      zsdtfl_flote_ref-saldo_disponivel = zsdtfl_flote_ref-saldo_disponivel + lv_qtd_vinc_dif.

                    ENDIF.
* Marcar registro para criação de linha nova.
                    PERFORM zf_set_line_to_new USING zsdtfl_flote_ref.
* Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE, ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF.
                    PERFORM zf_number_get_next USING    '01'
                                                        'ZSEQFLREF'
                                               CHANGING zsdtfl_flote_ref-seq.

                  ELSE.
                    ROLLBACK WORK.

                  ENDIF.

                  lv_rc = sy-subrc.

                ELSE.
* Verifica se a Finalidade de Exportação não é Finalidade de Retorno de Recusa.
                  IF el_export-finalidade NOT IN rl_final_ret_r.
                    MOVE: tl_vinc_p_flote-docnum_flote TO zsdtfl_flote_ref-docnum_flote,
                          tl_vinc_p_flote-docnum_eprod TO zsdtfl_flote_ref-docnum_eprod,
                          lv_qtd_vinc_dif              TO zsdtfl_flote_ref-qtd,
                          0                            TO zsdtfl_flote_ref-qtd_vinc,
                          lv_qtd_vinc_dif              TO zsdtfl_flote_ref-saldo_disponivel.
* Marcar registro para criação de linha nova.
                    PERFORM zf_set_line_to_new USING zsdtfl_flote_ref.
* Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE, ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF.
                    PERFORM zf_number_get_next USING    '01'
                                                        'ZSEQFLREF'
                                               CHANGING zsdtfl_flote_ref-seq.

                    lv_rc = 0.

                  ENDIF.

                ENDIF.

                IF NOT zsdtfl_flote_ref IS INITIAL AND
                       lv_rc            IS INITIAL.
                  MODIFY zsdtfl_flote_ref.
* Efetiva ou retorno manutenção de tabela transparente.
                  PERFORM zf_commit_or_rollback_tbl USING sy-subrc.

                ELSE.
* Efetiva ou retorno manutenção de tabela transparente.
                  PERFORM zf_commit_or_rollback_tbl USING lv_rc.

                ENDIF.

              ELSE.
* Efetiva ou retorno manutenção de tabela transparente.
                PERFORM zf_commit_or_rollback_tbl USING lv_rc.

              ENDIF.

            ELSE.
              ROLLBACK WORK.

            ENDIF.

          ELSE.
* Efetiva ou retorno manutenção de tabela transparente.
            PERFORM zf_commit_or_rollback_tbl USING lv_rc.

          ENDIF.

        ELSE.
          ROLLBACK WORK.

        ENDIF.

      ENDIF.

    ENDLOOP.

    CLEAR: lt_ret_flote, lt_export_up, tl_prod_flote, lt_prod_flote_up, tl_flote_flote,
           lt_flote_flote_up, lt_flote_flote_up2, tl_vinc_p_flote, lt_vinc_up,
           tl_vinc_p_flote[], tl_flote_flote[], tl_prod_flote[], zsdtflote_flote,
           *zsdtflote_flote.
* Por motivo de processamento em massa, é possível que dados processados e feito o COMMIT ainda não tenham sido
* efetivados na tabela física. Por conta disso, faz-se necessário o comando abaixo.
    COMMIT WORK AND WAIT.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_calc_saldos_nf_recusada
*&---------------------------------------------------------------------*
*& alcular valores de saldos da NF Recusada
*&---------------------------------------------------------------------*
*& --> PT_VINC_UP                     TI Tab. Vinculo de Lote Atualiza
*& --> PT_VINC                        TI Tab. Vinculo de Lote
*& --> PT_P_FL_FLOTE                  TI Tab. Prod. FLote ou FLote Flote
*& --> PT_P_FL_FLOTE_UP               TI Tab. Prod. FLote ou FLote Flote Atualiza
*& --> PR_FINAL_RET_R                 Finalidade de Exportação de Retoreno de Recusa
*& --> TL_FL_FLOTE_REF                TI Tab. FLote Flote Referência
*& --> UV_TBX_VINC                    Índice de linha da TI Vinculo de Lote
*& --> UV_TABIX                       Índice de linha da TI Ret. Lote
*& --> UV_SUBTRAENDO                  Quantidade a ser subtraída nas Qtds.
*& --> UV_FINALIDADE                  Finalidade de Exportação
*& --> UE_VINCLOTE                    Estrutura da Tab. Vinculo de Lote
*& --> UE_PROD_FLOTE                  Estrutura da Tab. Prod. FLote ou FLote Flote
*& <-- CV_NFRET_MENGE                 Qtd. NF de Retorno
*& <-- CV_RETLOTE_QUANT_VINC          Qtd. Vinc. da TI Ret. Lote
*& <-- CV_VINCLOTE_QTD_VINC           Qtd. Vinc. da TI Vinculo Lote
*& <-- CV_P_FL_FLOTE_SALDO_VINC       Sld. Vinc. TI PROD Lote ou FLOTE FLote
*& <-- CV_P_LF_FLOTE_SALDO_DISPONIVEL Sld. Disp. TI PROD Lote ou FLOTE FLote
*& --> CV_EXIT                        Indicador pára LOOP TI Vinculo Lote
*& --> CV_EXIT2                       Indicador pára LOOP TI Ret. Lote
*& --> CV_EXIT_DO                     Indicador pára Laço DO...ENDDO
*& --> CV_EXPORT                      Indicador atualiza Tabela Export
*&---------------------------------------------------------------------*
FORM zf_calc_saldos_nf_recusada TABLES pt_vinc_up                     TYPE STANDARD TABLE
                                       pt_vinc                        TYPE STANDARD TABLE
                                       pt_p_fl_flote                  TYPE STANDARD TABLE
                                       pt_p_fl_flote_up               TYPE STANDARD TABLE
                                       pt_fl_flote_ref                STRUCTURE zsdtfl_flote_ref
                                       pr_final_ret_r                 TYPE STANDARD TABLE
                                 USING uv_tbx_vinc                    TYPE sytabix
                                       uv_tabix                       TYPE sytabix
                                       uv_subtraendo                  TYPE j_1bnetqty
                                       uv_finalidade                  TYPE zfin_export
                                       ue_vinclote                    TYPE any
                                       ue_p_fl_flote                  TYPE any
                              CHANGING cv_nfret_menge                 TYPE j_1bnetqty
                                       cv_retlote_quant_vinc          TYPE j_1bnetqty
                                       cv_vinclote_qtd_vinc           TYPE zqtd_vinc
                                       cv_p_fl_flote_saldo_vinc       TYPE zsaldo_vinc
                                       cv_p_lf_flote_saldo_disponivel TYPE zsaldo_disponivel
                                       cv_exit                        TYPE c
                                       cv_exit2                       TYPE c
                                       cv_exit_do                     TYPE c
                                       cv_export                      TYPE c.

  CONSTANTS: lc_docnum_flote TYPE char30        VALUE 'DOCNUM_FLOTE',
             lc_docnum_eprod TYPE char30        VALUE 'DOCNUM_EPROD',
             lc_docnum_ref   TYPE char30        VALUE 'DOCNUM_REF',
             lc_docnum       TYPE char30        VALUE 'DOCNUM',
             lc_qtd_vinc     TYPE char30        VALUE 'QTD_VINC',
             lc_id_vinc      TYPE char30        VALUE 'ID_VINC',
             lc_vinc_virtual TYPE char30        VALUE 'VINC_VIRTUAL',
             lc_saldo_vinc   TYPE char30        VALUE 'SALDO_VINC',
             lc_saldo_dispon TYPE char30        VALUE 'SALDO_DISPONIVEL',
             lv_vlr_zeros    TYPE zdocnum_flote VALUE '0000000000'.

  cv_nfret_menge                 = cv_nfret_menge                 - uv_subtraendo.
  cv_retlote_quant_vinc          = cv_retlote_quant_vinc          - uv_subtraendo.
  cv_vinclote_qtd_vinc           = cv_vinclote_qtd_vinc           - uv_subtraendo.
* Verifica se a Finalidade de Exportação é uma Finalidade de Retorno de Recusa.
  IF uv_finalidade IN pr_final_ret_r.
    cv_p_fl_flote_saldo_vinc       = cv_p_fl_flote_saldo_vinc       - uv_subtraendo.
    cv_p_lf_flote_saldo_disponivel = cv_p_lf_flote_saldo_disponivel + uv_subtraendo.

  ENDIF.
* Verifica se é um Vinculo Virtual e se não é Finalidade de Retorno de Recusa.
  IF ue_vinclote-(lc_vinc_virtual) IS NOT INITIAL       AND
     uv_finalidade                 NOT IN pr_final_ret_r.
    cv_p_fl_flote_saldo_vinc = cv_p_fl_flote_saldo_vinc - uv_subtraendo.

  ENDIF.

  READ TABLE pt_vinc_up TRANSPORTING NO FIELDS WITH KEY (lc_docnum_flote) = ue_vinclote-(lc_docnum_flote)
                                                        (lc_docnum_eprod) = ue_vinclote-(lc_docnum_eprod)
                                                        (lc_id_vinc)      = ue_vinclote-(lc_id_vinc).

  IF sy-subrc IS INITIAL.
    MODIFY pt_vinc_up FROM ue_vinclote INDEX sy-tabix.

  ELSE.
    APPEND ue_vinclote TO pt_vinc_up.

  ENDIF.

  MODIFY pt_vinc FROM ue_vinclote INDEX uv_tbx_vinc.
* Verifica se já há dados para atualizar a tabela Notas de Produtor ou de Formação de Lote para Vincular.
  READ TABLE pt_p_fl_flote_up ASSIGNING FIELD-SYMBOL(<fs_p_fl_flote_up>) WITH KEY (lc_docnum) = ue_p_fl_flote-(lc_docnum).

  IF sy-subrc IS INITIAL.
* Verifica se a Finalidade de Exportação é uma Finalidade de Retorno de Recusa.
    IF uv_finalidade IN pr_final_ret_r.
      MODIFY pt_p_fl_flote_up FROM ue_p_fl_flote INDEX sy-tabix.

    ELSE.
      ADD: ue_p_fl_flote-(lc_saldo_vinc)   TO <fs_p_fl_flote_up>-(lc_saldo_vinc),
           ue_p_fl_flote-(lc_saldo_dispon) TO <fs_p_fl_flote_up>-(lc_saldo_dispon).

    ENDIF.

  ELSE.
    APPEND ue_p_fl_flote TO pt_p_fl_flote_up.

  ENDIF.
* Verifica se a Finalidade de Exportação é uma Finalidade de Retorno de Recusa.
  IF uv_finalidade IN pr_final_ret_r.
    MODIFY pt_p_fl_flote FROM ue_p_fl_flote INDEX uv_tabix.

  ENDIF.
* Processa registro de documento de referência na ZSDTFLOTE_FLOTE.
  READ TABLE pt_vinc WITH KEY (lc_docnum_ref) = ue_p_fl_flote-(lc_docnum).

  IF sy-subrc IS INITIAL.
    DATA(lv_subtraendo) = uv_subtraendo.

    LOOP AT pt_vinc FROM sy-tabix.

      IF pt_vinc-(lc_docnum_ref) EQ ue_p_fl_flote-(lc_docnum).
        LOOP AT pt_fl_flote_ref WHERE docnum_flote EQ pt_vinc-(lc_docnum_ref)
                                  AND docnum_eprod EQ pt_vinc-(lc_docnum_eprod).

          READ TABLE pt_vinc_up WITH KEY (lc_docnum_flote) = ue_vinclote-(lc_docnum_flote)
                                         (lc_docnum_eprod) = pt_fl_flote_ref-docnum_eprod
                                         (lc_docnum_ref)   = pt_fl_flote_ref-docnum_flote.

          IF sy-subrc IS INITIAL.
            IF pt_vinc_up-(lc_qtd_vinc) LE lv_subtraendo.
              pt_vinc_up-(lc_qtd_vinc) = pt_vinc_up-(lc_qtd_vinc) - pt_vinc_up-(lc_qtd_vinc).

            ELSE.
              pt_vinc_up-(lc_qtd_vinc) = pt_vinc_up-(lc_qtd_vinc) - lv_subtraendo.

            ENDIF.

            MODIFY pt_vinc_up INDEX sy-tabix.

          ELSE.
            pt_vinc_up-(lc_docnum_flote) = ue_vinclote-(lc_docnum_flote).
            pt_vinc_up-(lc_docnum_eprod) = pt_fl_flote_ref-docnum_eprod.
            pt_vinc_up-(lc_docnum_ref)   = pt_fl_flote_ref-docnum_flote.
            pt_vinc_up-(lc_id_vinc)      = pt_vinc-(lc_id_vinc).

            IF pt_fl_flote_ref-qtd_vinc LE lv_subtraendo.
              pt_vinc_up-(lc_qtd_vinc) = pt_fl_flote_ref-qtd_vinc - pt_fl_flote_ref-qtd_vinc.
              lv_subtraendo            = lv_subtraendo - pt_fl_flote_ref-qtd_vinc.

            ELSE.
              IF lv_subtraendo LE pt_vinc-(lc_qtd_vinc).
                pt_vinc_up-(lc_qtd_vinc) = pt_vinc-(lc_qtd_vinc) - lv_subtraendo.
                lv_subtraendo            = lv_subtraendo - lv_subtraendo.

              ELSE.
                pt_vinc_up-(lc_qtd_vinc) = lv_subtraendo - pt_vinc-(lc_qtd_vinc).
                lv_subtraendo            = lv_subtraendo - pt_vinc-(lc_qtd_vinc).

              ENDIF.

            ENDIF.

            APPEND pt_vinc_up.

          ENDIF.

          IF lv_subtraendo IS INITIAL.
            DATA(lv_exec) = abap_on.
            EXIT.

          ENDIF.

        ENDLOOP.

      ELSE.
        EXIT.

      ENDIF.

      IF lv_subtraendo IS INITIAL.
        lv_exec = abap_on.
        EXIT.

      ENDIF.

    ENDLOOP.

  ENDIF.
* Verifica se é um registro Virtual da ZSDTFLOTE_FLOTE.
  IF NOT ue_vinclote-(lc_vinc_virtual) IS INITIAL AND
         lv_exec                       IS INITIAL.
    READ TABLE pt_vinc WITH KEY (lc_docnum_flote) = ue_vinclote-(lc_docnum_flote)
                                (lc_docnum_ref)   = ue_vinclote-(lc_docnum_eprod).

    IF sy-subrc IS INITIAL.
      lv_subtraendo = uv_subtraendo.

      LOOP AT pt_vinc FROM sy-tabix.
        IF pt_vinc-(lc_docnum_flote) EQ ue_vinclote-(lc_docnum_flote) AND
           pt_vinc-(lc_docnum_ref)   EQ ue_vinclote-(lc_docnum_eprod).

          READ TABLE pt_vinc_up WITH KEY (lc_docnum_flote) = pt_vinc-(lc_docnum_flote)
                                         (lc_docnum_eprod) = pt_vinc-(lc_docnum_eprod)
                                         (lc_docnum_ref)   = pt_vinc-(lc_docnum_ref).

          IF sy-subrc IS INITIAL.
            IF pt_vinc_up-(lc_qtd_vinc) LE lv_subtraendo.
              pt_vinc_up-(lc_qtd_vinc) = pt_vinc_up-(lc_qtd_vinc) - pt_vinc_up-(lc_qtd_vinc).
              lv_subtraendo = lv_subtraendo - pt_vinc_up-(lc_qtd_vinc).

            ELSE.
              pt_vinc_up-(lc_qtd_vinc) = pt_vinc_up-(lc_qtd_vinc) - lv_subtraendo.
              lv_subtraendo = lv_subtraendo - lv_subtraendo.

            ENDIF.

            MODIFY pt_vinc_up INDEX sy-tabix.

          ELSE.
            pt_vinc_up-(lc_docnum_flote) = pt_vinc-(lc_docnum_flote).
            pt_vinc_up-(lc_docnum_eprod) = pt_vinc-(lc_docnum_eprod).
            pt_vinc_up-(lc_docnum_ref)   = pt_vinc-(lc_docnum_ref).
            pt_vinc_up-(lc_id_vinc)      = pt_vinc-(lc_id_vinc).


            IF pt_vinc-(lc_qtd_vinc) LE lv_subtraendo.
              pt_vinc_up-(lc_qtd_vinc) = pt_vinc-(lc_qtd_vinc) - pt_vinc-(lc_qtd_vinc).
              lv_subtraendo            = lv_subtraendo - pt_vinc-(lc_qtd_vinc).

            ELSE.
              pt_vinc_up-(lc_qtd_vinc) = pt_vinc-(lc_qtd_vinc) - lv_subtraendo.
              lv_subtraendo            = lv_subtraendo - lv_subtraendo.

            ENDIF.

            APPEND pt_vinc_up.

          ENDIF.

          IF lv_subtraendo IS INITIAL.
            EXIT.

          ENDIF.

        ELSE.
          EXIT.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.

  IF cv_nfret_menge        IS INITIAL OR
     cv_retlote_quant_vinc IS INITIAL.
    cv_exit    = abap_on.
    cv_exit2   = abap_on.
    cv_export  = abap_on.
    cv_exit_do = abap_on.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_update_tab_export
*&---------------------------------------------------------------------*
*& Prepara a atualização da Tabela Export
*&---------------------------------------------------------------------*
*&      --> PT_EXPORT_UP  Validar a Quantidade Exportada Tabela
*&      --> UE_EXPORT     Validar a Quantidade Exportada Estrutura
*&---------------------------------------------------------------------*
FORM zf_update_tab_export TABLES pt_export_up STRUCTURE zsdt_export
                          USING  ue_export    TYPE      zsdt_export.

  ue_export-process_1x1 = abap_on.
  READ TABLE pt_export_up TRANSPORTING NO FIELDS WITH KEY docnum    = ue_export-docnum
                                                          werks     = ue_export-werks
                                                          id_export = ue_export-id_export.

  IF sy-subrc IS INITIAL.
    MODIFY pt_export_up FROM ue_export INDEX sy-tabix TRANSPORTING process_1x1.

  ELSE.
    APPEND ue_export TO pt_export_up.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_number_get_next
*&---------------------------------------------------------------------*
*& Gera o número sequencial do campo SEQ das tabelas ZSDTPROD_FLOTE,
*& ZSDTFLOTE_FLOTE e ZSDTFL_FLOTE_REF
*&---------------------------------------------------------------------*
*&      -->UV_NR_RANGE Número do intervalo numérico
*&      -->UV_OBJECT   Nome do objeto de intervalo numérico
*&      <--CV_NUMBER   Retorno do p´róximo número livre do intervalo
*&---------------------------------------------------------------------*
FORM zf_number_get_next  USING    uv_nr_range TYPE nrnr
                                  uv_object   TYPE nrobj
                         CHANGING cv_number.

  DATA: lv_returncode TYPE nrreturn.
* Get ID Romaneio number ranger.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = uv_nr_range
      object                  = uv_object
    IMPORTING
      number                  = cv_number
      returncode              = lv_returncode
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_commit_or_rollback_tbl
*&---------------------------------------------------------------------*
*& Efetiva ou retorno manutenção de tabela transparente
*&---------------------------------------------------------------------*
*&      --> UV_RC Código de retorno
*&---------------------------------------------------------------------*
FORM zf_commit_or_rollback_tbl USING uv_rc TYPE sysubrc.

  IF uv_rc IS INITIAL.
    COMMIT WORK.

  ELSE.
    ROLLBACK WORK.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_set_line_to_cancel
*&---------------------------------------------------------------------*
*& Marcar registro para cancelamento
*&---------------------------------------------------------------------*
*&      --> UE_WORKAREA Estrutura em que será marcada para cancelar.
*&---------------------------------------------------------------------*
FORM zf_set_line_to_cancel USING ue_workarea TYPE any.

  CONSTANTS: lc_cancel    TYPE char30 VALUE 'CANCEL',
             lc_us_cancel TYPE char30 VALUE 'US_CANCEL',
             lc_dt_cancel TYPE char30 VALUE 'DT_CANCEL',
             lc_hr_cancel TYPE char30 VALUE 'HR_CANCEL'.

  ue_workarea-(lc_cancel)    = abap_on.
  ue_workarea-(lc_us_cancel) = sy-uname.
  ue_workarea-(lc_dt_cancel) = sy-datlo.
  ue_workarea-(lc_hr_cancel) = sy-timlo.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_set_line_to_new
*&---------------------------------------------------------------------*
*& Marcar registro para criação de linha nova
*&---------------------------------------------------------------------*
*&      --> UE_WORKAREA Estrutura em que será marcada para cancelar.
*&---------------------------------------------------------------------*
FORM zf_set_line_to_new USING ue_workarea TYPE any.
  CONSTANTS: lc_us_criacao TYPE char30 VALUE 'US_CRIACAO',
             lc_dt_criacao TYPE char30 VALUE 'DT_CRIACAO',
             lc_hr_criacao TYPE char30 VALUE 'HR_CRIACAO',
             lc_cancel     TYPE char30 VALUE 'CANCEL',
             lc_us_cancel  TYPE char30 VALUE 'US_CANCEL',
             lc_dt_cancel  TYPE char30 VALUE 'DT_CANCEL',
             lc_hr_cancel  TYPE char30 VALUE 'HR_CANCEL'.

  CLEAR: ue_workarea-(lc_cancel), ue_workarea-(lc_us_cancel), ue_workarea-(lc_dt_cancel), ue_workarea-(lc_hr_cancel).

  ue_workarea-(lc_us_criacao) = sy-uname.
  ue_workarea-(lc_dt_criacao) = sy-datum.
  ue_workarea-(lc_hr_criacao) = sy-uzeit.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_define_subtract_calc
*&---------------------------------------------------------------------*
*& Definição do subtraendo conforme quantidade envolvidas
*&---------------------------------------------------------------------*
*& --> PR_FINAL_RET_R           Range Finalidade de Exportação Ret. Recusa
*& --> CV_NFRET_MENGE           Qtd. NF de Retorno
*& --> CV_RETLOTE_QUANT_VINC    Qtd. Vinc. da TI Ret. Lote
*& --> CV_P_FL_FLOTE_SALDO_VINC Sld. Vinc. TI PROD Lote ou FLOTE FLote (Usado Final. <> U)
*& --> CV_VINCLOTE_QTD_VINC     Qtd. Vinc. da TI Vinculo Lote
*& --> UV_FINALIDADE            Finalidade de Exportação
*& <-- UV_SUBTRAENDO            Quantidade a ser subtraída nas Qtds.
*&---------------------------------------------------------------------*
FORM zf_define_subtract_calc TABLES   pr_final_ret_r
                             USING    uv_nfret_menge           TYPE j_1bnetqty
                                      uv_retlote_quant_vinc    TYPE j_1bnetqty
                                      uv_p_fl_flote_saldo_vinc TYPE zsaldo_vinc
                                      uv_vinclote_qtd_vinc     TYPE zqtd_vinc
                                      uv_finalidade            TYPE zfin_export
                             CHANGING cv_subtraendo            TYPE j_1bnetqty.

  IF uv_finalidade IN pr_final_ret_r.

    IF uv_nfret_menge LE uv_retlote_quant_vinc.

      IF uv_nfret_menge LE uv_p_fl_flote_saldo_vinc.

        IF uv_nfret_menge LE uv_vinclote_qtd_vinc.
          cv_subtraendo = uv_nfret_menge.

        ELSE.
          cv_subtraendo = uv_vinclote_qtd_vinc.

        ENDIF.

      ELSE.
        IF uv_vinclote_qtd_vinc LE uv_p_fl_flote_saldo_vinc.
          cv_subtraendo = uv_vinclote_qtd_vinc.

        ELSE.
          cv_subtraendo = uv_p_fl_flote_saldo_vinc.

        ENDIF.

      ENDIF.

    ELSE.
      IF uv_p_fl_flote_saldo_vinc LE uv_retlote_quant_vinc.

        IF uv_vinclote_qtd_vinc LE uv_p_fl_flote_saldo_vinc.
          cv_subtraendo = uv_vinclote_qtd_vinc.

        ELSE.
          cv_subtraendo = uv_p_fl_flote_saldo_vinc.

        ENDIF.

      ELSE.
        IF uv_retlote_quant_vinc LE uv_vinclote_qtd_vinc.
          cv_subtraendo = uv_retlote_quant_vinc.

        ELSE.
          cv_subtraendo = uv_vinclote_qtd_vinc.

        ENDIF.

      ENDIF.

    ENDIF.

  ELSE.
    IF uv_nfret_menge LE uv_retlote_quant_vinc.

      IF uv_nfret_menge LE uv_vinclote_qtd_vinc.
        cv_subtraendo = uv_nfret_menge.

      ELSE.
        cv_subtraendo = uv_vinclote_qtd_vinc.

      ENDIF.

    ELSE.
      IF uv_retlote_quant_vinc LE uv_vinclote_qtd_vinc.
        cv_subtraendo = uv_retlote_quant_vinc.

      ELSE.
        cv_subtraendo = uv_vinclote_qtd_vinc.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_reg_check_flote_flote
*&---------------------------------------------------------------------*
*& Valida se o Registro processado é um FLOTE_FLOTE
*&---------------------------------------------------------------------*
*& --> PT_FLOTE_FLOTE_UP  Notas de formação de lote Recusada para Vincular (Usado Final. = U)
*& --> PT_FLOTE_FLOTE_UP2 Notas de formação de lote Recusada para Vincular (Usado Final. <> U)
*& --> PR_FINAL_RET_R     Range Finalidade de Exportação Ret. Recusa
*& --> UV_FINALIDADE      Finalidade de Exportação
*& --> UE_DOCNUM_1        Docnum Nota Fiscal 1
*& --> UE_DOCNUM_2        Docnum Nota Fiscal 2
*& <-- CV_RC              Código de retorno
*&---------------------------------------------------------------------*
FORM zf_reg_check_flote_flote  TABLES   pt_flote_flote_up  TYPE STANDARD TABLE
                                        pt_flote_flote_up2 TYPE STANDARD TABLE
                                        pr_final_ret_r     TYPE STANDARD TABLE
                               USING    uv_finalidade      TYPE zfin_export
                                        ue_docnum_1        TYPE zdocnum_eprod
                                        ue_docnum_2        TYPE zdocnum_flote
                               CHANGING cv_rc              TYPE sysubrc.

  CONSTANTS: lc_docnum TYPE char30 VALUE 'DOCNUM'.

* Verifica se a Finalidade de Exportação é Finalidade de Retorno de Recusa.
  IF uv_finalidade IN pr_final_ret_r.
    READ TABLE pt_flote_flote_up TRANSPORTING NO FIELDS WITH KEY (lc_docnum) = ue_docnum_1.

  ELSE.
    READ TABLE pt_flote_flote_up2 TRANSPORTING NO FIELDS WITH KEY (lc_docnum) = ue_docnum_2.

  ENDIF.

  cv_rc = sy-subrc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_read_tab_zsdtfl_flote_ref
*&---------------------------------------------------------------------*
*& Lê a Tabela auxiliar - Controle do Saldo do Liberado pelo Retorno
*&---------------------------------------------------------------------*
*&      --> UE_DOCNUM_FLOTE Docnum Formação de Lote
*&      --> UE_DOCNUM_EPROD Docnum Nota Entrada Produtor
*&      <-- CE_FL_FLOTE_REF Tbl. Aux - Controle do Saldo do Liberado pelo Retorno
*&      <-- CV_RC           Código de retorno
*&---------------------------------------------------------------------*
FORM zf_read_tab_zsdtfl_flote_ref USING    ue_docnum_flote  TYPE zdocnum_flote
                                           ue_docnum_eprod  TYPE zdocnum_eprod
                                  CHANGING ce_fl_flote_ref  TYPE zsdtfl_flote_ref
                                           cv_rc            TYPE sysubrc.

  CLEAR ce_fl_flote_ref.
  SELECT SINGLE * FROM zsdtfl_flote_ref
    INTO ce_fl_flote_ref
  WHERE docnum_flote EQ ue_docnum_flote
    AND docnum_eprod EQ ue_docnum_eprod
    AND cancel       EQ space.

  cv_rc = sy-subrc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_read_tab_zsdtflole_flote
*&---------------------------------------------------------------------*
*& Lê a tabela Notas de formação de lote Recusada para Vincular
*&---------------------------------------------------------------------*
*&      --> UE_DOCNUM      Nº documento
*&      <-- CE_FLOTE_FLOTE Tabela Notas de formação de lote Recusada para Vincular
*&      <-- CV_RC          Código de retorno
*&---------------------------------------------------------------------*
FORM zf_read_tab_zsdtflote_flote USING    ue_docnum      TYPE j_1bdocnum
                                 CHANGING ce_flote_flote TYPE zsdtflote_flote
                                          cv_rc          TYPE sysubrc.

* Busca dados da Tabela de formação de lote para atualização da tabela quando a SALDO_VINC for modificado.
  SELECT SINGLE * FROM zsdtflote_flote
    INTO ce_flote_flote
  WHERE docnum EQ ue_docnum
    AND cancel EQ space.

  cv_rc = sy-subrc.

ENDFORM.
