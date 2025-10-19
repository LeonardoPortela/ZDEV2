FUNCTION z_les_tipo_remessa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_VBELN) TYPE  VBELN_VL
*"  CHANGING
*"     VALUE(P_PARID_AG) TYPE  J_1BPARID OPTIONAL
*"     VALUE(P_PARID_RM) TYPE  J_1BPARID OPTIONAL
*"     VALUE(P_PARID_WE) TYPE  J_1BPARID OPTIONAL
*"     VALUE(P_PARID_PC) TYPE  J_1BPARID OPTIONAL
*"     VALUE(P_PARID_LR) TYPE  J_1BPARID OPTIONAL
*"     VALUE(P_PARID_RG) TYPE  J_1BPARID OPTIONAL
*"     VALUE(P_PARID_Z1) TYPE  J_1BPARID OPTIONAL
*"     VALUE(VG_VSTEL) TYPE  CHAR01 OPTIONAL
*"     VALUE(CFOP_FRETE) TYPE  J_1BCFOP OPTIONAL
*"     VALUE(COND_PAGA) TYPE  DZTERM OPTIONAL
*"     VALUE(FORM_PAGA) TYPE  SCHZW_BSEG OPTIONAL
*"----------------------------------------------------------------------

  DATA: vg_parc     TYPE j_1bparid,
        vg_brach_ag TYPE j_1bbranch,
        vg_brach_rm TYPE j_1bbranch,
        mt_lips     TYPE TABLE OF lips WITH HEADER LINE,
        i_xvbadr    TYPE TABLE OF sadrvb WITH HEADER LINE,
        i_xvbpa	    TYPE TABLE OF vbpavb WITH HEADER LINE,
        " Parceiro aviso recebimento
        i_xvbadr_a  TYPE TABLE OF sadrvb WITH HEADER LINE,
        i_xvbpa_a	  TYPE TABLE OF vbpavb WITH HEADER LINE,
        vg_vbkd     TYPE vbkd,
        vg_vbap     TYPE vbap.

  CLEAR: p_parid_ag,
         p_parid_rm,
         p_parid_we,
         p_parid_pc,
         p_parid_lr,
         p_parid_rg,
         vg_vstel,
         cfop_frete.

  vg_vstel  = 'X'.

  SELECT * INTO TABLE mt_lips
    FROM lips
   WHERE vbeln = p_vbeln.

  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE * INTO @DATA(wa_likp)
    FROM likp
   WHERE vbeln EQ @p_vbeln.

  CHECK sy-subrc IS INITIAL.

  READ TABLE mt_lips INDEX 1.

  SELECT SINGLE * INTO vg_vbkd
    FROM vbkd
   WHERE vbeln EQ mt_lips-vgbel
     AND posnr EQ mt_lips-vgpos.

  cond_paga = vg_vbkd-zterm.
  form_paga = vg_vbkd-zlsch.

  SELECT SINGLE * INTO vg_vbap
    FROM vbap
   WHERE vbeln EQ mt_lips-vgbel
     AND posnr EQ mt_lips-vgpos.

  cfop_frete = vg_vbap-j_1bcfop.

  CALL FUNCTION 'SD_PARTNER_READ'
    EXPORTING
      f_vbeln  = mt_lips-vgbel
      object   = 'VBPA'
    TABLES
      i_xvbadr = i_xvbadr
      i_xvbpa  = i_xvbpa.

  CALL FUNCTION 'SD_PARTNER_READ' " Aviso de recebimento
    EXPORTING
      f_vbeln  = mt_lips-vbeln
      object   = 'VBPA'
    TABLES
      i_xvbadr = i_xvbadr_a
      i_xvbpa  = i_xvbpa_a.

  READ TABLE i_xvbpa WITH KEY parvw = 'AG'.
  IF sy-subrc IS INITIAL.
    p_parid_ag    = i_xvbpa-kunnr.
  ENDIF.

  READ TABLE i_xvbpa WITH KEY parvw = 'PC'.
  IF sy-subrc IS INITIAL.
    p_parid_pc = i_xvbpa-lifnr.
  ELSE.
    READ TABLE i_xvbpa_a WITH KEY parvw = 'PC'. " Aviso de recebimento
    IF sy-subrc IS INITIAL.
      p_parid_pc = i_xvbpa_a-lifnr.
    ENDIF.
  ENDIF.

  READ TABLE i_xvbpa WITH KEY parvw = 'LR'.
  IF sy-subrc IS INITIAL.
    p_parid_lr = i_xvbpa-kunnr.
  ENDIF.

  READ TABLE i_xvbpa WITH KEY parvw = 'WE'.
  IF sy-subrc IS INITIAL.
    p_parid_we = i_xvbpa-kunnr.
  ELSE.
    READ TABLE i_xvbpa_a WITH KEY parvw = 'WE' INTO DATA(wa_xvbpa_a).
    IF sy-subrc IS INITIAL.
      p_parid_we = wa_xvbpa_a-kunnr.
    ENDIF.
  ENDIF.

  READ TABLE i_xvbpa WITH KEY parvw = 'RG'.
  IF sy-subrc IS INITIAL.
    p_parid_rg = i_xvbpa-kunnr.
  ENDIF.

  READ TABLE i_xvbpa WITH KEY parvw = 'RM'.
  IF sy-subrc IS INITIAL.
    p_parid_rm    = i_xvbpa-lifnr.
  ELSE.
    IF wa_likp-vbtyp EQ 'T' .
      READ TABLE i_xvbpa WITH KEY parvw = 'AG' INTO DATA(wa_xvbpa).
      IF sy-subrc IS INITIAL.
        TRY .
            p_parid_rm =
            CAST zcl_fornecedores(
            zcl_fornecedores=>zif_parceiros~get_instance(
              )->set_parceiro_cnpj_cpf_ie( EXPORTING i_cnpj = CONV #( wa_xvbpa-stcd1 ) i_cpf = CONV #( wa_xvbpa-stcd2 ) i_insc_estatual = wa_xvbpa-stcd3
              ) )->at_lfa1-lifnr.
          CATCH zcx_parceiros.
        ENDTRY.
      ENDIF.
    ELSE.
      CLEAR: vg_vstel.
      p_parid_rm = mt_lips-werks.
*-CS2021001045 - 18.04.2022 - JT - inicio
      SELECT chave_nf_venda
        INTO @DATA(l_chave_nf_venda)
          UP TO 1 ROWS
        FROM zlest0210
       WHERE remessa_dummy = @mt_lips-vbeln.
      ENDSELECT.

      IF sy-subrc = 0.
        p_parid_rm = p_parid_pc.
      ENDIF.
*-CS2021001045 - 18.04.2022 - JT - inicio
    ENDIF.
  ENDIF.

  READ TABLE i_xvbpa WITH KEY parvw = 'Z1'.
  IF sy-subrc IS INITIAL.
    p_parid_z1 = i_xvbpa-lifnr.
  ENDIF.

  IF NOT vg_vstel IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = p_parid_ag
      IMPORTING
        output = vg_parc.

    IF strlen( vg_parc ) LE 4.

      vg_brach_ag-branch = vg_parc.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vg_brach_ag-branch
        IMPORTING
          output = vg_brach_ag-branch.

      SELECT SINGLE * INTO vg_brach_ag
        FROM j_1bbranch
       WHERE branch EQ vg_brach_ag-branch.

      IF sy-subrc IS INITIAL.
        CLEAR: vg_vstel.
      ENDIF.
    ENDIF.

    CHECK NOT vg_vstel IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = p_parid_rm
      IMPORTING
        output = vg_parc.

    IF strlen( vg_parc ) LE 4.

      vg_brach_rm-branch = vg_parc.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vg_brach_rm-branch
        IMPORTING
          output = vg_brach_rm-branch.

      SELECT SINGLE * INTO vg_brach_rm
        FROM j_1bbranch
       WHERE branch EQ vg_brach_rm-branch.

      IF sy-subrc IS INITIAL.
        CLEAR: vg_vstel.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFUNCTION.
