*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0205_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zlest0205_exit.

FORM f_exit_zlest0205_0002 USING p_registro_manter TYPE zlest0205
                           CHANGING p_error.

*-Proj.EUDR-10.10.2024-#154904-JT-inicio
  DATA: w_tvarv             TYPE tvarvc,
        ls_zib_nfe_dist_ter TYPE zib_nfe_dist_ter,
        ls_zib_nfe_dist_itm TYPE zib_nfe_dist_itm,
        lc_filial           TYPE char01,
        lc_matkl            TYPE mara-matkl,
        lc_matnr            TYPE mara-matnr,
        lc_erro             TYPE char01.
*-Proj.EUDR-10.10.2024-#154904-JT-fim

  IF p_registro_manter-nfe IS NOT INITIAL.
    IF p_registro_manter-bukrs IS INITIAL.
      MESSAGE 'Campo Empresa obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-branch IS INITIAL.
      MESSAGE 'Campo Filial obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-nr_safra IS INITIAL.
      MESSAGE 'Campo Safra obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-matnr   IS INITIAL.
      MESSAGE 'Campo Material obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-local_descarga IS INITIAL.
      MESSAGE 'Campo Local Descarga obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ENDIF.
  ELSE.
    IF p_registro_manter-bukrs IS INITIAL.
      MESSAGE 'Campo Empresa obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-branch IS INITIAL.
      MESSAGE 'Campo Filial obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-parid IS INITIAL.
      MESSAGE 'Campo Parceiro obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-nr_safra IS INITIAL.
      MESSAGE 'Campo Safra obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-matnr   IS INITIAL.
      MESSAGE 'Campo Material obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-peso_fiscal IS INITIAL.
      MESSAGE 'Campo Peso Fiscal obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-nfnum IS INITIAL.
      MESSAGE 'Campo Nota Fiscal obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-series IS INITIAL.
      MESSAGE 'Campo Série obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-docdat IS INITIAL.
      MESSAGE 'Campo Data Emissão obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-netwr IS INITIAL.
      MESSAGE 'Campo Valor da NF obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-peso_subtotal IS INITIAL.
      MESSAGE 'Campo Peso Subtotal obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-local_descarga IS INITIAL.
      MESSAGE 'Campo Local Descarga obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ELSEIF p_registro_manter-cfop IS INITIAL.
      MESSAGE 'Campo CFOP obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ENDIF.
*    IF p_error IS INITIAL.
*      IF p_registro_manter-cfop IS NOT INITIAL.
*        SELECT SINGLE cfop INTO p_registro_manter-cfop
*          FROM  j_1bagn
*          WHERE version = 2
*            AND cfop = p_registro_manter-cfop.
*        IF sy-subrc NE 0.
*          MESSAGE 'CFOP não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
*          p_error = 'X'.
*        ENDIF.
*      ENDIF.
*    ENDIF.

  ENDIF.

  IF p_error IS INITIAL.
    IF p_registro_manter-bukrs IS NOT INITIAL.
      SELECT SINGLE bukrs INTO p_registro_manter-bukrs
        FROM t001
        WHERE bukrs = p_registro_manter-bukrs.
      IF sy-subrc NE 0.
        MESSAGE 'Empresa não encontrada!' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
      ENDIF.
    ENDIF.

    IF p_registro_manter-branch IS NOT INITIAL.
      SELECT SINGLE branch INTO p_registro_manter-branch
        FROM j_1bbranch
        WHERE branch = p_registro_manter-branch.
      IF sy-subrc NE 0.
        MESSAGE 'Filial não encontrada!' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
  IF p_error IS INITIAL.
    IF p_registro_manter-branch IS NOT INITIAL AND
       p_registro_manter-bukrs IS NOT INITIAL.
      SELECT SINGLE branch INTO p_registro_manter-branch
        FROM j_1bbranch
        WHERE branch = p_registro_manter-branch
          AND bukrs = p_registro_manter-bukrs.
      IF sy-subrc NE 0.
        MESSAGE 'Filial não pertence a empresa informada!' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF p_registro_manter-matnr   IS NOT INITIAL.
      SELECT SINGLE matnr INTO p_registro_manter-matnr
        FROM makt
        WHERE matnr = p_registro_manter-matnr.
      IF sy-subrc NE 0.
        MESSAGE 'Material não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF p_registro_manter-parid   IS NOT INITIAL.
      SELECT SINGLE lifnr INTO p_registro_manter-parid
        FROM lfa1
        WHERE lifnr = p_registro_manter-parid.
      IF sy-subrc NE 0.
        MESSAGE 'Fornecedor não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF p_registro_manter-local_descarga  IS NOT INITIAL.
      SELECT SINGLE id_local_entrega INTO p_registro_manter-local_descarga
        FROM zsdt0001le
        WHERE id_local_entrega = p_registro_manter-local_descarga.
      IF sy-subrc NE 0.
        MESSAGE 'Local de Descarga não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_error IS INITIAL.
    IF p_registro_manter-tp_transgenia   IS NOT INITIAL.

      DATA: g_domain TYPE dd07l-domname.
      DATA: g_value  TYPE dd07l-domvalue_l.
      DATA: g_subrc TYPE sy-subrc,
            gwa_tab TYPE dd07v.

      g_domain = 'ZDM_TP_TRANG'.
      g_value  = p_registro_manter-tp_transgenia.

      CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
        EXPORTING
          domname  = g_domain
          value    = g_value
        IMPORTING
          dd07v_wa = gwa_tab
          rc       = g_subrc.
      IF g_subrc NE 0.
        MESSAGE 'Transgeniase não encontrado!' TYPE 'S' DISPLAY LIKE 'E'.
        p_error = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

*-IR220401-31.01.2025-#165417-JT-inicio - comentado
*-Proj.EUDR-10.10.2024-#154904-JT-inicio
*  IF p_error IS INITIAL.
*    PERFORM f_validar_cnpj_material    USING p_registro_manter-chave_nfe
*                                    CHANGING lc_filial
*                                             lc_matkl
*                                             lc_matnr
*                                             p_error.
*    IF p_error = abap_true.
*      MESSAGE s024(sd) WITH 'Grupo de Material: ' lc_matkl ' não esta na TVARV "MAGGI_GR_GRAOS" !' DISPLAY LIKE 'E'.
*    ENDIF.
*  ENDIF.
*-IR220401-31.01.2025-#165417-JT-fim - comentado

  IF p_error IS INITIAL.
    g_domain = 'ZEUDR'.
    g_value  = p_registro_manter-eudr.

    CALL FUNCTION 'DD_DOMVALUE_TEXT_GET'
      EXPORTING
        domname  = g_domain
        value    = g_value
      IMPORTING
        dd07v_wa = gwa_tab
        rc       = g_subrc.
    IF g_subrc NE 0.
      MESSAGE 'Cod EUDR Incorreto!' TYPE 'S' DISPLAY LIKE 'E'.
      p_error = 'X'.
    ENDIF.
  ENDIF.
*-Proj.EUDR-10.10.2024-#154904-JT-fim

ENDFORM.

FORM f_validar_cnpj_material    USING p_chave_nfe
                             CHANGING p_filial
                                      p_matkl
                                      p_matnr
                                      p_erro.

  DATA: lc_matnr  TYPE mara-matnr.

  FREE: p_filial, p_matkl, p_matnr, p_erro.

  SELECT SINGLE forne_cnpj
    INTO @DATA(_forne_cnpj)
    FROM zib_nfe_dist_ter
   WHERE chave_nfe = @p_chave_nfe.

  IF sy-subrc = 0.
    SELECT SINGLE stcd1
      INTO @DATA(_stcd1)
      FROM j_1bbranch
     WHERE stcd1 = @_forne_cnpj.

    IF sy-subrc = 0.
      SELECT SINGLE prod_codigo
        INTO @DATA(_prod_codigo)
        FROM zib_nfe_dist_itm
       WHERE chave_nfe = @p_chave_nfe.

      IF sy-subrc = 0.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = _prod_codigo
          IMPORTING
            output = lc_matnr.

        SELECT SINGLE matkl
          INTO @DATA(_matkl)
          FROM mara
         WHERE matnr = @lc_matnr.

        IF sy-subrc = 0.
*-#165579-04.02.2025-JT-inicio
*          SELECT SINGLE low
*            INTO @DATA(_low)
*            FROM tvarvc
*           WHERE name = 'MAGGI_GR_GRAOS'
*             AND low  = @_matkl.
*
*          IF sy-subrc = 0.
          p_filial = abap_true.
          p_matnr  = lc_matnr.
*          ELSE.
*            p_erro   = abap_true.
*            p_matkl  = _matkl.
*          ENDIF.
*-#165579-04.02.2025-JT-fim
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_buscar_faturamento    USING p_chave_nfe
                          CHANGING p_ch_referencia
                                   p_nr_safra
                                   p_remessa
                                   p_material
                                   p_propria_emissao.  "*-US 159346-25.11.2024-JT-inicio

  DATA: zcl_util      TYPE REF TO zcl_util,
        lc_campos_nfe TYPE zde_campos_nfe,
        lc_filial     TYPE char01,
        lc_matkl      TYPE mara-matkl,
        lc_matnr      TYPE mara-matnr,
        lc_erro       TYPE char01,
        t_active      TYPE TABLE OF j_1bnfe_active,
        w_active      TYPE j_1bnfe_active.

  FREE: p_ch_referencia, p_nr_safra, p_material, p_propria_emissao. "*-US 159346-25.11.2024-JT-inicio

  CREATE OBJECT zcl_util.

  PERFORM f_validar_cnpj_material    USING p_chave_nfe
                                  CHANGING lc_filial
                                           lc_matkl
                                           p_material
                                           lc_erro.

  CHECK lc_filial = abap_true.

  lc_campos_nfe  = zcl_util->get_atributos_nfe( p_chave_nfe ).

  SELECT SINGLE j_1bnfe_active~docnum,
                j_1bnfe_active~form          "*-US 159346-25.11.2024-JT-inicio
    INTO @DATA(_j_1bnfe_active)              "*-US 159346-25.11.2024-JT-inicio
    FROM j_1bnfe_active
   INNER JOIN j_1bnfdoc        ON j_1bnfdoc~docnum = j_1bnfe_active~docnum
   WHERE j_1bnfe_active~regio   = @lc_campos_nfe-regio
     AND j_1bnfe_active~nfyear  = @lc_campos_nfe-nfyear
     AND j_1bnfe_active~nfmonth = @lc_campos_nfe-nfmonth
     AND j_1bnfe_active~stcd1   = @lc_campos_nfe-stcd1
     AND j_1bnfe_active~model   = @lc_campos_nfe-model
     AND j_1bnfe_active~serie   = @lc_campos_nfe-serie
     AND j_1bnfe_active~nfnum9  = @lc_campos_nfe-nfnum9
     AND j_1bnfe_active~docnum9 = @lc_campos_nfe-docnum9
     AND j_1bnfe_active~cdv     = @lc_campos_nfe-cdv
     AND j_1bnfdoc~direct       = '2'
     AND j_1bnfdoc~cancel       = @abap_off.

  CHECK sy-subrc = 0.

  p_propria_emissao = COND #( WHEN _j_1bnfe_active-form = 'NF55' THEN abap_true    "*-US 159346-25.11.2024-JT-inicio
                                                                 ELSE abap_false ).

  SELECT SINGLE refkey, reftyp
    INTO @DATA(_j_1bnflin)
    FROM j_1bnflin
   WHERE docnum = @_j_1bnfe_active-docnum.  "*-US 159346-25.11.2024-JT-inicio

  CHECK sy-subrc = 0 AND _j_1bnflin-reftyp = 'BI'.

  SELECT SINGLE vbelv
    INTO @DATA(_vbelv)
    FROM vbfa
   WHERE vbeln = @_j_1bnflin-refkey
     AND vbtyp_v = 'J'.

  CHECK sy-subrc = 0.

  SELECT SINGLE ch_referencia, nr_safra
    INTO @DATA(_zsdt0001)
    FROM zsdt0001
   WHERE doc_rem      = @_vbelv
     AND tp_movimento = 'S'.

  CHECK sy-subrc = 0.

  p_ch_referencia = _zsdt0001-ch_referencia.
  p_nr_safra      = _zsdt0001-nr_safra.
  p_remessa       = _vbelv.

ENDFORM.

FORM f_exit_zlest0205_0005 CHANGING p_registro_manter TYPE zlest0205.

  DATA: ls_zib_nfe_dist_ter TYPE zib_nfe_dist_ter.
  DATA: ls_zib_nfe_dist_itm TYPE zib_nfe_dist_itm.
  DATA: ls_zib_nfe_dist_tvo TYPE zib_nfe_dist_tvo.  "*-#165579-04.02.2025-JT-inicio

  IF p_registro_manter-matnr IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_registro_manter-matnr
      IMPORTING
        output = p_registro_manter-matnr.
  ENDIF.
  IF p_registro_manter-parid IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_registro_manter-parid
      IMPORTING
        output = p_registro_manter-parid.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-name(23) = '<FS_WA_REGISTRO_MANTER>'.
      screen-input = 1.
      MODIFY SCREEN.
    ENDIF.
*-US 159346-25.11.2024-JT-inicio
    IF screen-name = '<FS_WA_REGISTRO_MANTER>-PROPRIA_EMISSAO'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
*-US 159346-25.11.2024-JT-fim
  ENDLOOP.

  IF sy-ucomm = 'CHANGE'.

    DATA lv_vinc_tot_aquav TYPE char01.

    SELECT SINGLE vinc_tot_aquav INTO lv_vinc_tot_aquav
    FROM zlest0205
    WHERE chave_nfe = p_registro_manter-chave_nfe.

    IF lv_vinc_tot_aquav IS NOT INITIAL.
      LOOP AT SCREEN.
        FIND 'FS_WA_REGISTRO_MANTER' IN screen-name.
        IF sy-subrc = 0.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
      MESSAGE 'Nota vinculada a comboio aquaviário. Não permite alteração!' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      LOOP AT SCREEN.
        IF screen-name = '<FS_WA_REGISTRO_MANTER>-PESO_FISCAL' OR
           screen-name = '<FS_WA_REGISTRO_MANTER>-NFNUM' OR
           screen-name = '<FS_WA_REGISTRO_MANTER>-SERIES' OR
           screen-name = '<FS_WA_REGISTRO_MANTER>-DOCDAT' OR
           screen-name = '<FS_WA_REGISTRO_MANTER>-NETWR' OR
           screen-name = '<FS_WA_REGISTRO_MANTER>-PESO_SUBTOTAL' OR
           screen-name = '<FS_WA_REGISTRO_MANTER>-CFOP' OR
           screen-name = '<FS_WA_REGISTRO_MANTER>-PARID'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name = '<FS_WA_REGISTRO_MANTER>-PESO_FISCAL' OR
         screen-name = '<FS_WA_REGISTRO_MANTER>-NFNUM' OR
         screen-name = '<FS_WA_REGISTRO_MANTER>-SERIES' OR
         screen-name = '<FS_WA_REGISTRO_MANTER>-DOCDAT' OR
         screen-name = '<FS_WA_REGISTRO_MANTER>-NETWR' OR
         screen-name = '<FS_WA_REGISTRO_MANTER>-PESO_SUBTOTAL' OR
         screen-name = '<FS_WA_REGISTRO_MANTER>-CFOP'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    IF p_registro_manter-nfe IS NOT INITIAL.
      IF p_registro_manter-chave_nfe IS NOT INITIAL.

        SELECT SINGLE * INTO ls_zib_nfe_dist_ter
          FROM zib_nfe_dist_ter
          WHERE chave_nfe = p_registro_manter-chave_nfe.
        IF sy-subrc = 0.

*-#165579-04.02.2025-JT-inicio
          SELECT SINGLE * INTO ls_zib_nfe_dist_tvo
            FROM zib_nfe_dist_tvo
            WHERE chave_nfe = p_registro_manter-chave_nfe.
*-#165579-04.02.2025-JT-fim

          SELECT SINGLE * INTO ls_zib_nfe_dist_itm
            FROM zib_nfe_dist_itm
            WHERE chave_nfe = p_registro_manter-chave_nfe.

          DATA(obj) = NEW zcl_fornecedores( ).

          obj->zif_parceiros~set_parceiro_cnpj_cpf_ie(
              EXPORTING
                i_cnpj             = CONV #( ls_zib_nfe_dist_ter-forne_cnpj )
                i_insc_estatual    = CONV #( ls_zib_nfe_dist_ter-forne_ie )         "*-IR220401-31.01.2025-#165417-JT
             )->get_id_parceiro( IMPORTING e_parceiro = p_registro_manter-parid ).

*          SELECT SINGLE lifnr INTO p_registro_manter-parid
*            FROM lfa1
*            WHERE stcd1  =  ls_zib_nfe_dist_ter-forne_cnpj.

* IR075719
          IF  ls_zib_nfe_dist_itm-prod_und_trib EQ 'TN' OR
              ls_zib_nfe_dist_itm-prod_und_trib EQ 'TO' OR
              ls_zib_nfe_dist_itm-prod_und_trib EQ 'TON'.
            ls_zib_nfe_dist_itm-prod_qtd_comerci = ls_zib_nfe_dist_itm-prod_qtd_comerci * 1000.
          ENDIF.
* IR075719

          p_registro_manter-peso_fiscal   = ls_zib_nfe_dist_tvo-nm_pesob. "ls_zib_nfe_dist_itm-prod_qtd_comerci. "*-#165579-04.02.2025-JT-inicio
          p_registro_manter-nfnum         = ls_zib_nfe_dist_ter-numero.
          p_registro_manter-series        = ls_zib_nfe_dist_ter-serie.
          p_registro_manter-docdat        = ls_zib_nfe_dist_ter-dt_emissao.
          p_registro_manter-netwr         = ls_zib_nfe_dist_ter-vl_total_fatura.
          p_registro_manter-peso_subtotal = ls_zib_nfe_dist_tvo-nm_pesob. "ls_zib_nfe_dist_itm-prod_qtd_comerci. "*-#165579-04.02.2025-JT-inicio
          p_registro_manter-cfop          = ls_zib_nfe_dist_itm-prod_cfop.

          LOOP AT SCREEN.
            IF screen-name = '<FS_WA_REGISTRO_MANTER>-PESO_FISCAL' OR
               screen-name = '<FS_WA_REGISTRO_MANTER>-NFNUM' OR
               screen-name = '<FS_WA_REGISTRO_MANTER>-SERIES' OR
               screen-name = '<FS_WA_REGISTRO_MANTER>-DOCDAT' OR
               screen-name = '<FS_WA_REGISTRO_MANTER>-NETWR' OR
               screen-name = '<FS_WA_REGISTRO_MANTER>-PESO_SUBTOTAL' OR
               screen-name = '<FS_WA_REGISTRO_MANTER>-CFOP' OR
               screen-name = '<FS_WA_REGISTRO_MANTER>-PARID'.
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.

          ENDLOOP.

        ELSE.
          MESSAGE 'Chave NFe não encontrada!' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ELSE.
        CLEAR p_registro_manter.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

FORM f_exit_zlest0205_0006 USING p_saida TYPE zlest0205_out
                           CHANGING p_error.
  DATA ls_zlest0205 TYPE zlest0205.

  SELECT SINGLE * INTO ls_zlest0205
    FROM zlest0205
    WHERE chave_nfe = p_saida-chave_nfe.

  IF  ls_zlest0205-vinc_tot_aquav IS NOT INITIAL.
    MESSAGE 'Nota vinculada a comboio aquaviário. Não permite deleção!' TYPE 'S' DISPLAY LIKE 'E'.
    p_error = 'X'.
  ENDIF.
ENDFORM.

*-Proj.EUDR-10.10.2024-#154904-JT-inicio
FORM f_exit_zlest0205_0017 USING p_tipo.

  IF p_tipo = '0001'.
    PERFORM f4_val_param_espec USING '<FS_WA_REGISTRO_MANTER>-EUDR'.
  ENDIF.

ENDFORM.

FORM f4_val_param_espec USING p_cod  TYPE help_info-dynprofld.

  TYPES: BEGIN OF ty_dados,
           eudr  TYPE zeudr,
           descr TYPE char60,
         END OF ty_dados.

  DATA: t_dd07v   TYPE TABLE OF dd07v,
        t_dados   TYPE TABLE OF ty_dados,
        t_mapping TYPE STANDARD TABLE OF dselc,
        s_mapping TYPE dselc,
        t_ret     TYPE TABLE OF ddshretval,
        v_rc      TYPE sy-subrc.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname   = 'ZEUDR'
      text      = 'X'
      langu     = sy-langu
*     BYPASS_BUFFER        = ' '
    IMPORTING
      rc        = v_rc
    TABLES
      dd07v_tab = t_dd07v.

  LOOP AT t_dd07v INTO DATA(ls_dd07v).
    APPEND VALUE #( eudr = ls_dd07v-domvalue_l descr = ls_dd07v-ddtext ) TO t_dados.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'EUDR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = p_cod
      window_title    = 'Atende EUDR'
      value_org       = 'S'
    TABLES
      value_tab       = t_dados
      return_tab      = t_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.

FORM f_exit_zlest0205_0008 CHANGING p_col_pos
                                    p_ref_tabname
                                    p_ref_fieldname
                                    p_tabname
                                    p_field
                                    p_scrtext_l
                                    p_outputlen
                                    p_edit
                                    p_sum
                                    p_emphasize
                                    p_just
                                    p_hotspot
                                    p_f4
                                    p_check.

  IF p_ref_tabname = 'ZLEST0205_OUT' AND  "*-US 159346-25.11.2024-JT-inicio
     p_field       = 'PROPRIA_EMISSAO'.
    p_check        = abap_true.
  ENDIF.

ENDFORM.

FORM f_exit_zlest0205_0016 USING p_ucomm  TYPE sy-ucomm CHANGING p_registro_manter TYPE any p_saida TYPE any.

  DATA: w_zlest0205        TYPE zlest0205,
        lc_ch_referencia   TYPE zlest0205-ch_referencia,
        lc_nr_safra        TYPE zlest0205-nr_safra,
        lc_remessa         TYPE zsdt0001-doc_rem,
        lc_material        TYPE zsdt0001-matnr,
        lc_propria_emissao TYPE zlest0205-propria_emissao. "*-US 159346-25.11.2024-JT-inicio

  CLEAR: w_zlest0205.
*
  MOVE-CORRESPONDING p_registro_manter TO w_zlest0205.

  IF w_zlest0205-chave_nfe IS NOT INITIAL AND
     w_zlest0205-nfe       IS NOT INITIAL.
    PERFORM f_buscar_faturamento    USING w_zlest0205-chave_nfe
                                 CHANGING lc_ch_referencia
                                          lc_nr_safra
                                          lc_remessa
                                          lc_material
                                          lc_propria_emissao.  "*-US 159346-25.11.2024-JT-inicio
  ENDIF.

  w_zlest0205-propria_emissao = lc_propria_emissao. "*-US 159346-25.11.2024-JT-inicio

  IF w_zlest0205-ch_referencia IS INITIAL.
    w_zlest0205-ch_referencia = lc_ch_referencia.
  ENDIF.

  IF w_zlest0205-nr_safra      IS INITIAL.
    w_zlest0205-nr_safra      = lc_nr_safra.
  ENDIF.

  IF w_zlest0205-doc_rem       IS INITIAL.
    w_zlest0205-doc_rem       = lc_remessa.
  ENDIF.

  IF w_zlest0205-matnr         IS INITIAL.
    w_zlest0205-matnr         = lc_material.
  ENDIF.

  MOVE-CORRESPONDING w_zlest0205       TO p_registro_manter.

ENDFORM.
*-Proj.EUDR-10.10.2024-#154904-JT-fim
*-US 156544-19-11-2024-#156544-RJF-Inicio
FORM f_exit_zlest0205_0013 TABLES p_table.

  CALL TRANSACTION 'ZLES0195'.

ENDFORM.
*-US 156544-19-11-2024-#156544-RJF-Fim
