*----------------------------------------------------------------------*
***INCLUDE LZLES_FAT_CONTINGENCIAF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_preenche_dados_fat_rom
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_CH_REFERENCIA
*&      <-- LWA_DADOS_FATURAMENTO
*&---------------------------------------------------------------------*
FORM f_preenche_dados_fat_rom  USING p_ch_referencia TYPE zsdt0001-ch_referencia
                            CHANGING c_dados_faturamento TYPE zde_compare_faturamento.

  DATA: lva_docnum_mdfe TYPE j_1bnfdoc-docnum.

  CLEAR: c_dados_faturamento.

  SELECT SINGLE *
    FROM zsdt0001 INTO @DATA(lwa_zsdt0001)
   WHERE ch_referencia = @p_ch_referencia.

  CHECK sy-subrc EQ 0.

  c_dados_faturamento-ch_referencia                = lwa_zsdt0001-ch_referencia.
  c_dados_faturamento-vbeln                        = lwa_zsdt0001-vbeln.

  PERFORM f_set_dados_docnum USING lwa_zsdt0001-nro_nf_prod
                            CHANGING c_dados_faturamento-chave_nfe
                                     c_dados_faturamento-data_lcto_nf
                                     c_dados_faturamento-netwr_nf
                                     c_dados_faturamento-authcode_nfe
                                     c_dados_faturamento-nfenum_nfe.

  PERFORM f_set_dados_docnum USING lwa_zsdt0001-nro_nf_frete
                            CHANGING c_dados_faturamento-chave_cte
                                     c_dados_faturamento-data_lcto_cte
                                     c_dados_faturamento-netwr_cte
                                     c_dados_faturamento-authcode_cte
                                     c_dados_faturamento-nfenum_cte.

  PERFORM f_get_docnum_mdfe USING lwa_zsdt0001-nro_nf_prod
                                  lwa_zsdt0001-nro_nf_frete
                                  lwa_zsdt0001-nro_nf_rem
                         CHANGING lva_docnum_mdfe.

  IF lva_docnum_mdfe IS NOT INITIAL.
    SELECT SINGLE *
      FROM zsdt0102 INTO @DATA(lws_zsdt0102)
     WHERE docnum EQ @lva_docnum_mdfe.

    IF sy-subrc EQ 0.
      c_dados_faturamento-mdfe_encerrado = lws_zsdt0102-encerrado.
    ENDIF.
  ENDIF.

  PERFORM f_set_dados_docnum USING lva_docnum_mdfe
                          CHANGING c_dados_faturamento-chave_mdfe
                                   c_dados_faturamento-data_lcto_mdfe
                                   c_dados_faturamento-netwr_mdfe
                                   c_dados_faturamento-authcode_mdfe
                                   c_dados_faturamento-nfenum_mdfe.

  PERFORM f_set_info_vt USING lwa_zsdt0001-doc_transp
                           CHANGING c_dados_faturamento.

  PERFORM f_set_info_tip USING lwa_zsdt0001-nro_nf_frete
                      CHANGING c_dados_faturamento.

  PERFORM f_set_info_averbacao USING lwa_zsdt0001-nro_nf_prod
                                     lwa_zsdt0001-nro_nf_frete
                            CHANGING c_dados_faturamento.

  IF lwa_zsdt0001-seq_lcto IS NOT INITIAL.
    SELECT SINGLE *
      FROM zfiwrt0008 INTO @DATA(lwa_zfiwrt0008)
     WHERE seq_lcto  = @lwa_zsdt0001-seq_lcto.

    IF sy-subrc EQ 0 AND lwa_zfiwrt0008-docnum IS NOT INITIAL.
      PERFORM f_set_dados_docnum USING lwa_zfiwrt0008-docnum
                              CHANGING c_dados_faturamento-chave_nfe_rem
                                       c_dados_faturamento-data_lcto_nf_rem
                                       c_dados_faturamento-netwr_nf_rem
                                       c_dados_faturamento-authcode_nfe_rem
                                       c_dados_faturamento-nfenum_nfe_nfw.

    ENDIF.
  ENDIF.

  IF lwa_zsdt0001-doc_material IS NOT INITIAL.
    SELECT SINGLE *
      FROM mkpf INTO @DATA(lwa_mkpf)
     WHERE mblnr  = @lwa_zsdt0001-doc_material.

    IF sy-subrc EQ 0.
      CONCATENATE lwa_mkpf-mblnr lwa_mkpf-mjahr INTO DATA(lwa_refkey).

      SELECT SINGLE *
        FROM j_1bnflin INTO @DATA(lwa_lin)
       WHERE refkey = @lwa_refkey.

      IF sy-subrc EQ 0 AND lwa_refkey IS NOT INITIAL AND lwa_lin-docnum IS NOT INITIAL.
        PERFORM f_set_dados_docnum USING lwa_lin-docnum
                                CHANGING c_dados_faturamento-chave_nfe_arm
                                         c_dados_faturamento-data_lcto_nf_arm
                                         c_dados_faturamento-netwr_nf_arm
                                         c_dados_faturamento-authcode_nfe_arm
                                         c_dados_faturamento-nfenum_nfe_arm.

      ENDIF.
    ENDIF.
  ENDIF.

  IF lwa_zsdt0001-doc_rem  IS NOT INITIAL.

    SELECT posnr , j_1bcfop
      FROM lips INTO TABLE @DATA(it_cfop)
      WHERE vbeln = @lwa_zsdt0001-doc_rem.

    LOOP AT it_cfop INTO DATA(lw_cfop).
      c_dados_faturamento-cfop = c_dados_faturamento-cfop && lw_cfop-j_1bcfop.
    ENDLOOP.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_chave_docnum
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_ZSDT0001_NRO_NF_PROD
*&      <-- C_DADOS_FATURAMENTO_CHAVE_NFE
*&---------------------------------------------------------------------*
FORM f_set_dados_docnum  USING p_docnum   TYPE j_1bnfdoc-docnum
                      CHANGING c_chave    TYPE zde_chave_doc_e
                               c_pstdat   TYPE j_1bnfdoc-pstdat
                               c_netwr    TYPE j_1bnfdoc-nftot
                               c_authcode TYPE j_1bnfeauthcode
                               c_nfenum   TYPE j_1bnfnum9.

  CHECK  p_docnum IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_docnum
    IMPORTING
      output = p_docnum.

  SELECT SINGLE *
    FROM j_1bnfdoc INTO @DATA(lwa_doc_nf)
   WHERE docnum  EQ @p_docnum.

  IF lwa_doc_nf-pstdat = lwa_doc_nf-docdat. "os 2 devem ser sempre iguais
    c_pstdat = lwa_doc_nf-pstdat.
  ENDIF.

  c_netwr  = lwa_doc_nf-nftot.
  c_nfenum = lwa_doc_nf-nfenum.

  SELECT SINGLE *
    FROM j_1bnfe_active INTO @DATA(lwa_active_nf)
   WHERE docnum  EQ @p_docnum.

  CHECK ( sy-subrc EQ 0 ) AND lwa_active_nf-docsta EQ '1' AND lwa_active_nf-scssta <> '2'.

  CHECK sy-subrc EQ 0 AND lwa_doc_nf-candat IS INITIAL.

  c_chave  = lwa_active_nf-regio &&
             lwa_active_nf-nfyear &&
             lwa_active_nf-nfmonth &&
             lwa_active_nf-stcd1 &&
             lwa_active_nf-model &&
             lwa_active_nf-serie &&
             lwa_active_nf-nfnum9 &&
             lwa_active_nf-docnum9 &&
             lwa_active_nf-cdv.

  SELECT SINGLE *
    FROM zib_nfe INTO @DATA(lwa_zib_nfe)
   WHERE docnum EQ @p_docnum.

  IF sy-subrc EQ 0.
    c_authcode = lwa_zib_nfe-authcod.
  ENDIF.

ENDFORM.

FORM f_get_docnum_mdfe  USING  p_docnum1 TYPE j_1bnfdoc-docnum
                               p_docnum2 TYPE j_1bnfdoc-docnum
                               p_docnum3 TYPE j_1bnfdoc-docnum
                      CHANGING c_docnum_mdfe TYPE j_1bnfdoc-docnum.

  DATA: it_zsdt0105 TYPE TABLE OF zsdt0105.

  CLEAR: c_docnum_mdfe, it_zsdt0105[].

  SELECT *
     INTO TABLE it_zsdt0105
     FROM zsdt0105
    WHERE docnum = p_docnum1.

  SELECT *
    APPENDING TABLE it_zsdt0105
    FROM zsdt0105
   WHERE docnum = p_docnum2.

  SELECT *
    APPENDING TABLE it_zsdt0105
    FROM zsdt0105
   WHERE docnum = p_docnum3.

  LOOP AT it_zsdt0105 INTO DATA(w_zsdt0105).
    SELECT SINGLE docnum
      INTO c_docnum_mdfe
      FROM zsdt0102
     WHERE docnum     = w_zsdt0105-docnum_ref
       AND autorizado = abap_true
       AND estornado  = abap_false
       AND cancel     = abap_false.
    IF ( sy-subrc = 0 ) AND ( c_docnum_mdfe IS NOT INITIAL  ).
      EXIT.
    ENDIF.
  ENDLOOP.




ENDFORM.

FORM f_set_info_vt  USING p_doc_transporte    TYPE vttk-tknum
                 CHANGING c_dados_faturamento TYPE zde_compare_faturamento.

  DATA: lit_vfkp TYPE TABLE OF vfkp,
        lit_konv TYPE TABLE OF konv.

  CHECK p_doc_transporte IS NOT INITIAL.

*--------------------------------------------------------------------------*
* Seta Informações Conditions
*--------------------------------------------------------------------------*

  CLEAR: c_dados_faturamento-kbetr_zfre,
         c_dados_faturamento-kbetr_ziof,
         c_dados_faturamento-kbetr_zseg.

  SELECT fknum fkpos waers kzwi1 prsdt knumv rebel FROM vfkp
  INTO CORRESPONDING FIELDS OF TABLE lit_vfkp
    WHERE rebel = p_doc_transporte
      AND fkpty = 'Z001'.

  IF lit_vfkp[] IS NOT INITIAL.
    SELECT
      FROM v_konv FIELDS *
      FOR ALL ENTRIES IN @lit_vfkp
     WHERE knumv EQ @lit_vfkp-knumv INTO CORRESPONDING FIELDS OF TABLE @lit_konv.

    LOOP AT lit_konv INTO DATA(lwa_konv) WHERE kschl = 'ZFRE' OR
                                               kschl = 'ZSEG' OR
                                               kschl = 'ZIOF' or
                                               kschl = 'ZPED' or
                                               kschl = 'ZADM' or
                                               kschl = 'ZPIS' or
                                               kschl = 'ZCOF'.




      CASE lwa_konv-kschl.
        WHEN 'ZFRE'.
          add lwa_konv-kwert to c_dados_faturamento-kbetr_zfre.
        WHEN 'ZSEG'.
          add lwa_konv-kwert to c_dados_faturamento-kbetr_zseg.
        WHEN 'ZIOF'.
          add lwa_konv-kwert to c_dados_faturamento-kbetr_ziof.
        WHEN 'ZPED'.
          add lwa_konv-kwert to c_dados_faturamento-kbetr_zped.
        WHEN 'ZADM'.
          add lwa_konv-kwert to c_dados_faturamento-kbetr_zadm.
        WHEN 'ZPIS'.
          add lwa_konv-kwert to c_dados_faturamento-kbetr_zpis.
        WHEN 'ZCOF'.
          add lwa_konv-kwert to c_dados_faturamento-kbetr_zcof.
      ENDCASE.
    ENDLOOP.
  ENDIF.

*--------------------------------------------------------------------------*
* Seta Informações Parceiros
*--------------------------------------------------------------------------*

  CLEAR: c_dados_faturamento-cpf_cnpj_pv,
         c_dados_faturamento-cpf_cnpj_mt,
         c_dados_faturamento-cpf_cnpj_lr,
         c_dados_faturamento-cpf_cnpj_sp,
         c_dados_faturamento-cpf_cnpj_pc,
         c_dados_faturamento-cpf_cnpj_sg.

  SELECT *
    FROM vtpa INTO TABLE @DATA(lit_vtpa)
   WHERE vbeln EQ @p_doc_transporte.

  LOOP AT lit_vtpa INTO DATA(lwa_vtpa).

    IF lwa_vtpa-kunnr IS NOT INITIAL.
      SELECT SINGLE *
       FROM kna1 INTO @DATA(lwa_kna1)
      WHERE kunnr = @lwa_vtpa-kunnr.

      CHECK sy-subrc EQ 0 AND lwa_vtpa-kunnr IS NOT INITIAL.
    ELSEIF  lwa_vtpa-lifnr IS NOT INITIAL.

      SELECT SINGLE *
       FROM lfa1 INTO @DATA(lwa_lfa1)
      WHERE lifnr = @lwa_vtpa-lifnr.

      CHECK sy-subrc EQ 0 AND lwa_vtpa-lifnr IS NOT INITIAL.
    ELSE.
      CONTINUE.
    ENDIF.

    CASE lwa_vtpa-parvw.
      WHEN 'LR'.

        IF lwa_kna1-stcd1 IS NOT INITIAL.
          c_dados_faturamento-cpf_cnpj_lr = lwa_kna1-stcd1.
        ELSEIF lwa_kna1-stcd2 IS NOT INITIAL..
          c_dados_faturamento-cpf_cnpj_lr = lwa_kna1-stcd2.
        ENDIF.

      WHEN 'SG'.

        IF lwa_kna1-stcd1 IS NOT INITIAL.
          c_dados_faturamento-cpf_cnpj_sg = lwa_kna1-stcd1.
        ELSEIF lwa_kna1-stcd2 IS NOT INITIAL..
          c_dados_faturamento-cpf_cnpj_sg = lwa_kna1-stcd2.
        ENDIF.


      WHEN 'PV'.

        IF lwa_lfa1-stcd1 IS NOT INITIAL.
          c_dados_faturamento-cpf_cnpj_pv = lwa_lfa1-stcd1.
        ELSEIF lwa_lfa1-stcd2 IS NOT INITIAL..
          c_dados_faturamento-cpf_cnpj_pv = lwa_lfa1-stcd2.
        ENDIF.

      WHEN 'MT'.

        IF lwa_lfa1-stcd1 IS NOT INITIAL.
          c_dados_faturamento-cpf_cnpj_mt = lwa_lfa1-stcd1.
        ELSEIF lwa_lfa1-stcd2 IS NOT INITIAL..
          c_dados_faturamento-cpf_cnpj_mt = lwa_lfa1-stcd2.
        ENDIF.

      WHEN 'SP'.

        IF lwa_lfa1-stcd1 IS NOT INITIAL.
          c_dados_faturamento-cpf_cnpj_sp = lwa_lfa1-stcd1.
        ELSEIF lwa_lfa1-stcd2 IS NOT INITIAL..
          c_dados_faturamento-cpf_cnpj_sp = lwa_lfa1-stcd2.
        ENDIF.

      WHEN 'PC'.

        IF lwa_lfa1-stcd1 IS NOT INITIAL.
          c_dados_faturamento-cpf_cnpj_pc = lwa_lfa1-stcd1.
        ELSEIF lwa_lfa1-stcd2 IS NOT INITIAL..
          c_dados_faturamento-cpf_cnpj_pc = lwa_lfa1-stcd2.
        ENDIF.

    ENDCASE.

  ENDLOOP.






ENDFORM.

FORM f_set_info_tip  USING   p_docnum           TYPE j_1bnfdoc-docnum
                    CHANGING c_dados_faturamento TYPE zde_compare_faturamento.

  CLEAR: c_dados_faturamento-tip_nucontrato,
         c_dados_faturamento-tip_link_contrato,
         c_dados_faturamento-tip_link_resumo,
         c_dados_faturamento-tip_link_pedagio.

  CHECK p_docnum IS NOT INITIAL.

  SELECT SINGLE *
    FROM zcte_ciot INTO @DATA(lwa_zcte_ciot)
   WHERE docnum = @p_docnum.

  CHECK sy-subrc EQ 0.

  c_dados_faturamento-tip_nucontrato         = lwa_zcte_ciot-nucontrato.
  c_dados_faturamento-tip_link_contrato      = lwa_zcte_ciot-link_contrato.
  c_dados_faturamento-tip_link_resumo        = lwa_zcte_ciot-link_resumo.
  c_dados_faturamento-tip_link_pedagio       = lwa_zcte_ciot-link_pedagio.
  c_dados_faturamento-tip_link_carga_pedagio = lwa_zcte_ciot-link_carga_pedagio.
  c_dados_faturamento-tip_nr_ciot            = lwa_zcte_ciot-nr_ciot.
  c_dados_faturamento-tip_st_ciot            = lwa_zcte_ciot-st_ciot.

ENDFORM.

FORM f_set_info_averbacao USING  p_docnum1 TYPE j_1bnfdoc-docnum
                                 p_docnum2 TYPE j_1bnfdoc-docnum
                    CHANGING c_dados_faturamento TYPE zde_compare_faturamento.

  DATA: lit_zlest0143 TYPE TABLE OF zlest0143.

  CLEAR: lit_zlest0143[],
         c_dados_faturamento-averb_nr_averbacao,
         c_dados_faturamento-averb_nr_protocolo.

  IF p_docnum1 IS NOT INITIAL.
    SELECT *
      FROM zlest0143 INTO TABLE lit_zlest0143
     WHERE docnum EQ p_docnum1.
  ENDIF.

  IF p_docnum2 IS NOT INITIAL.
    SELECT *
      FROM zlest0143 APPENDING TABLE lit_zlest0143
     WHERE docnum EQ p_docnum2.
  ENDIF.

  SORT lit_zlest0143 BY nr_averbacao.

  LOOP AT lit_zlest0143 INTO DATA(lwa_zlest0143).
    c_dados_faturamento-averb_nr_averbacao = lwa_zlest0143-nr_averbacao.
    c_dados_faturamento-averb_nr_protocolo = lwa_zlest0143-nr_protocolo.
    EXIT.
  ENDLOOP.

ENDFORM.


FORM f_montar_estrutura USING  VALUE(p_col_pos)       TYPE i
                               VALUE(p_ref_tabname)   LIKE dd02d-tabname
                               VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                               VALUE(p_tabname)       LIKE dd02d-tabname
                               VALUE(p_field)         LIKE dd03d-fieldname
                               VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                               VALUE(p_outputlen)
                               VALUE(p_hotspot).

  CLEAR gwa_estrutura.

  gwa_estrutura-fieldname     = p_field.
  gwa_estrutura-tabname       = p_tabname.
  gwa_estrutura-ref_tabname   = p_ref_tabname.
  gwa_estrutura-ref_fieldname = p_ref_fieldname.
  gwa_estrutura-key           = ' '.
  gwa_estrutura-key_sel       = 'X'.
  gwa_estrutura-col_pos       = p_col_pos.
  gwa_estrutura-no_out        = ' '.
  gwa_estrutura-seltext_s     = p_scrtext_l.
  gwa_estrutura-seltext_m     = p_scrtext_l.
  gwa_estrutura-seltext_l     = p_scrtext_l.
  gwa_estrutura-hotspot       = p_hotspot.

  IF p_scrtext_l IS NOT INITIAL.
    gwa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  TRANSLATE  gwa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  gwa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  gwa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  gwa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND gwa_estrutura TO git_estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM f_show_divergencia_faturamento  USING    p_dados_faturamento_s4 TYPE zde_compare_faturamento
                                              p_dados_faturamento_ecc TYPE zde_compare_faturamento.

  TYPES: BEGIN OF ty_compare_faturamento,
           origem_faturamento TYPE c LENGTH 50.
           INCLUDE STRUCTURE zde_compare_faturamento.
  TYPES:  END OF ty_compare_faturamento.

  DATA: lwa_report       LIKE sy-repid,
        lv_return        TYPE c,
        lwa_layout       TYPE slis_layout_alv,
        lit_faturamentos TYPE TABLE OF ty_compare_faturamento.

  CLEAR: lit_faturamentos[], git_estrutura[].

  APPEND INITIAL LINE TO lit_faturamentos ASSIGNING FIELD-SYMBOL(<fs_faturamento_s4>).
  MOVE-CORRESPONDING p_dados_faturamento_s4 TO <fs_faturamento_s4>.
  <fs_faturamento_s4>-origem_faturamento = 'S4'.

  APPEND INITIAL LINE TO lit_faturamentos ASSIGNING FIELD-SYMBOL(<fs_faturamento_ecc>).
  MOVE-CORRESPONDING p_dados_faturamento_ecc TO <fs_faturamento_ecc>.
  <fs_faturamento_ecc>-origem_faturamento = 'ECC'.

  PERFORM f_montar_estrutura USING:

   01   ''  ''              'LIT_FATURAMENTOS' 'ORIGEM_FATURAMENTO'       'Origem Faturamento'          '19' '',
   01   ''  ''              'LIT_FATURAMENTOS' 'CH_REFERENCIA'            'Ch.Ref.'                     '12' '',
   02   ''  ''              'LIT_FATURAMENTOS' 'VBELN'                    'Ordem/Pedido'                '12' '',

   03   ''  ''              'LIT_FATURAMENTOS' 'DATA_LCTO_NF'             'Data Lcto NF.'               '19' '',
   04   ''  ''              'LIT_FATURAMENTOS' 'NETWR_NF'                 'Valor NFe'                   '19' '',
   05   ''  ''              'LIT_FATURAMENTOS' 'NFENUM_NFE'               'Numero NFe'                  '19' '',
   06   ''  ''              'LIT_FATURAMENTOS' 'CHAVE_NFE'                'Chave NF-e'                  '44' '',
   07   ''  ''              'LIT_FATURAMENTOS' 'AUTHCODE_NFE'             'Prot.Aut.NFe'                '19' '',

   08   ''  ''              'LIT_FATURAMENTOS' 'DATA_LCTO_CTE'            'Data Lcto CTe'               '19' '',
   09   ''  ''              'LIT_FATURAMENTOS' 'NETWR_CTE'                'Valor CTe'                   '19' '',
   10   ''  ''              'LIT_FATURAMENTOS' 'NFENUM_CTE'               'Numero CTe'                  '19' '',
   11   ''  ''              'LIT_FATURAMENTOS' 'CHAVE_CTE'                'Chave CT-e'                  '44' '',
   12   ''  ''              'LIT_FATURAMENTOS' 'AUTHCODE_CTE'             'Prot.Aut.CTe'                '19' '',

   13   ''  ''              'LIT_FATURAMENTOS' 'KBETR_ZFRE'               'ZFRE - VI'                   '19' '',
   14   ''  ''              'LIT_FATURAMENTOS' 'KBETR_ZSEG'               'ZSEG - VI'                   '19' '',
   15   ''  ''              'LIT_FATURAMENTOS' 'KBETR_ZIOF'               'ZIOF - VI'                   '19' '',
   16   ''  ''              'LIT_FATURAMENTOS' 'KBETR_ZPED'               'ZPED - VI'                   '19' '',
   17   ''  ''              'LIT_FATURAMENTOS' 'KBETR_ZADM'               'ZADM - VI'                   '19' '',
   18   ''  ''              'LIT_FATURAMENTOS' 'KBETR_ZPIS'               'ZPIS - VI'                   '19' '',
   19   ''  ''              'LIT_FATURAMENTOS' 'KBETR_ZCOF'               'ZCOF - VI'                   '19' '',


   20   ''  ''              'LIT_FATURAMENTOS' 'CPF_CNPJ_PV'              'CPF/CNPJ PV'                 '14' '',
   21   ''  ''              'LIT_FATURAMENTOS' 'CPF_CNPJ_MT'              'CPF/CNPJ MT'                 '14' '',
   22   ''  ''              'LIT_FATURAMENTOS' 'CPF_CNPJ_LR'              'CPF/CNPJ LR'                 '14' '',
   23   ''  ''              'LIT_FATURAMENTOS' 'CPF_CNPJ_SP'              'CPF/CNPJ SP'                 '14' '',
   24   ''  ''              'LIT_FATURAMENTOS' 'CPF_CNPJ_PC'              'CPF/CNPJ PC'                 '14' '',
   25   ''  ''              'LIT_FATURAMENTOS' 'CPF_CNPJ_Z1'              'CPF/CNPJ Z1'                 '14' '',
   26   ''  ''              'LIT_FATURAMENTOS' 'CPF_CNPJ_SG'              'CPF/CNPJ SG'                 '14' '',

   27  ''  ''               'LIT_FATURAMENTOS' 'TIP_NUCONTRATO'           'Tip- NuContrato'             '19' '',
   28   ''  ''              'LIT_FATURAMENTOS' 'TIP_LINK_CONTRATO'        'Tip- Link Contrato'          '19' '',
   29   ''  ''              'LIT_FATURAMENTOS' 'TIP_LINK_RESUMO'          'Tip- Resumo'                 '19' '',
   30   ''  ''              'LIT_FATURAMENTOS' 'TIP_LINK_PEDAGIO'         'Tip- Link Pedagio'           '19' '',
   31   ''  ''              'LIT_FATURAMENTOS' 'TIP_LINK_CARGA_PEDAGIO'   'Tip- Link Carga Pedagio'     '19' '',
   32   ''  ''              'LIT_FATURAMENTOS' 'TIP_NR_CIOT'              'Nr. Ciot'                    '19' '',
   33   ''  ''              'LIT_FATURAMENTOS' 'TIP_ST_CIOT'              'St. Ciot'                    '19' '',


   34   ''  ''              'LIT_FATURAMENTOS' 'DATA_LCTO_MDFE'           'Data Lcto MDF-e'             '19' '',
   35   ''  ''              'LIT_FATURAMENTOS' 'NETWR_MDFE'               'Valor MDFe'                  '19' '',
   36   ''  ''              'LIT_FATURAMENTOS' 'NFENUM_MDFE'              'Numero MDFe'                 '19' '',

   37   ''  ''              'LIT_FATURAMENTOS' 'CHAVE_MDFE'               'Chave MDF-e'                 '44' '',
   38   ''  ''              'LIT_FATURAMENTOS' 'AVERB_NR_AVERBACAO'       'Nr. Averbação'               '19' '',
   39   ''  ''              'LIT_FATURAMENTOS' 'AVERB_NR_PROTOCOLO'       'Nr. Protocolo Averb.'        '19' '',
   40   ''  ''              'LIT_FATURAMENTOS' 'AUTHCODE_MDFE'            'Prot.Aut.MDFe'               '19' '',
   41   ''  ''              'LIT_FATURAMENTOS' 'MDFE_ENCERRADO'           'MDF-e Encerrado'             '19' '',

   42   ''  ''              'LIT_FATURAMENTOS' 'DATA_LCTO_NF_REM'         'Dt. Lcto ZNFW'               '19' '',
   43   ''  ''              'LIT_FATURAMENTOS' 'NETWR_NF_REM'             'Valor NF ZNFW'               '19' '',
   44   ''  ''              'LIT_FATURAMENTOS' 'NFENUM_NFE_NFW'           'Numero ZNFW'                 '19' '',
   45   ''  ''              'LIT_FATURAMENTOS' 'CHAVE_NFE_REM'            'Chave NFe ZNFW'              '19' '',
   46   ''  ''              'LIT_FATURAMENTOS' 'AUTHCODE_NFE_REM'         'Prot.Aut.ZNFW'               '19' '',

   47   ''  ''              'LIT_FATURAMENTOS' 'DATA_LCTO_NF_ARM'         'Dt. Lcto NF Arm.'            '19' '',
   48   ''  ''              'LIT_FATURAMENTOS' 'NETWR_NF_ARM'             'Valor NF Arm.'               '19' '',
   49   ''  ''              'LIT_FATURAMENTOS' 'NFENUM_NFE_ARM'           'Numero Arm'                  '19' '',
   50   ''  ''              'LIT_FATURAMENTOS' 'CHAVE_NFE_ARM'            'Chave NFe Arm.'              '19' '',
   51   ''  ''              'LIT_FATURAMENTOS' 'AUTHCODE_NFE_ARM'         'Prot.Aut.Arm'                '19' '',
   52   ''  ''              'LIT_FATURAMENTOS' 'CFOP'                     'CFOP'                        '6'  ''.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_grid_title       = 'Faturamentos'
      i_callback_program = lwa_report
      is_layout          = lwa_layout
      "i_callback_user_command = 'USER_COMMAND'
      it_fieldcat        = git_estrutura
    TABLES
      t_outtab           = lit_faturamentos.

ENDFORM.

FORM f_preenche_dados_fat_fre_int  USING p_vbeln TYPE zlest0108-vbeln
                               CHANGING c_dados_faturamento TYPE zde_compare_faturamento.

  DATA: lva_docnum_mdfe TYPE j_1bnfdoc-docnum.

  CLEAR: c_dados_faturamento.

  SELECT SINGLE *
    FROM zlest0108 INTO @DATA(lwa_zlest0108)
   WHERE vbeln = @p_vbeln.

  CHECK sy-subrc EQ 0.

  c_dados_faturamento-vbeln     = lwa_zlest0108-vbeln_ecc.

  PERFORM f_set_dados_docnum USING lwa_zlest0108-nro_nf_frete
                            CHANGING c_dados_faturamento-chave_cte
                                     c_dados_faturamento-data_lcto_cte
                                     c_dados_faturamento-netwr_cte
                                     c_dados_faturamento-authcode_cte
                                     c_dados_faturamento-nfenum_cte.

  PERFORM f_get_docnum_mdfe USING lwa_zlest0108-nro_nf_frete
                                  lwa_zlest0108-nro_nf_frete
                                  lwa_zlest0108-nro_nf_frete
                         CHANGING lva_docnum_mdfe.

  IF lva_docnum_mdfe IS NOT INITIAL.
    SELECT SINGLE *
      FROM zsdt0102 INTO @DATA(lws_zsdt0102)
     WHERE docnum EQ @lva_docnum_mdfe.

    IF sy-subrc EQ 0.
      c_dados_faturamento-mdfe_encerrado = lws_zsdt0102-encerrado.
    ENDIF.
  ENDIF.

  PERFORM f_set_dados_docnum USING lva_docnum_mdfe
                          CHANGING c_dados_faturamento-chave_mdfe
                                   c_dados_faturamento-data_lcto_mdfe
                                   c_dados_faturamento-netwr_mdfe
                                   c_dados_faturamento-authcode_mdfe
                                   c_dados_faturamento-nfenum_mdfe.

  PERFORM f_set_info_vt USING lwa_zlest0108-doc_transp
                           CHANGING c_dados_faturamento.

  PERFORM f_set_info_tip USING lwa_zlest0108-nro_nf_frete
                      CHANGING c_dados_faturamento.

  PERFORM f_set_info_averbacao USING lwa_zlest0108-nro_nf_frete
                                     lwa_zlest0108-nro_nf_frete
                            CHANGING c_dados_faturamento.

ENDFORM.
