"Name: \PR:HBRCCED0\FO:MOVE_DCC\SE:BEGIN\EI
ENHANCEMENT 0 Z_INFORME_IR.
* Inicio - alteração - RMNI -  CSTASK0012612 - 25.01.2023
  IF pn-begda(4) = '2022'.
    DATA lt_rgdir      TYPE STANDARD TABLE OF pc261.
    DATA lt_rgdir_res  TYPE STANDARD TABLE OF pc261.
    DATA lt_result     TYPE STANDARD TABLE OF pay99_result WITH HEADER LINE.
    DATA lt_wpbp TYPE STANDARD TABLE OF hrpay99_wpbp.          "RMNI 23.02.2023
    DATA lt_evp TYPE STANDARD TABLE OF pc261 WITH HEADER LINE.          "RMNI 23.02.2023
    DATA lt_evp_aux     TYPE STANDARD TABLE OF pc261 WITH HEADER LINE.
    DATA lt_inter      TYPE STANDARD TABLE OF pay99_international.
    DATA lt_rt         TYPE STANDARD TABLE OF hrpay99_rt .
*    DATA lt_compl_info TYPE STANDARD TABLE OF pbrdi_s_detail_cced WITH HEADER LINE.
    DATA lt_compl_info TYPE hrpaybr_t_cced_compl_inf.

    DATA ls_result     LIKE LINE OF lt_result.
    DATA ls_wpbp LIKE LINE OF lt_wpbp.           "RMNI 23.02.2023
    DATA ls_evp LIKE LINE OF lt_evp.           "RMNI 23.02.2023
    DATA ls_evp_aux LIKE LINE OF lt_evp_aux.           "RMNI 23.02.2023
    DATA ls_rt         LIKE LINE OF lt_rt.
    DATA ls_inter      LIKE LINE OF lt_inter.
    DATA ls_compl_info LIKE LINE OF lt_compl_info.

    DATA wa_compl_info             TYPE hrpaybr_s_cced_compl_inf.
    DATA: lv_position TYPE i.

    DATA lv_molga                  TYPE t500l-molga.
    DATA lv_relid                  TYPE t500l-relid.
    DATA lv_icatu                  TYPE maxbt.
    DATA lv_bradesco               TYPE maxbt.
    DATA lv_fmuni                  TYPE maxbt.
    DATA lv_bra_saude              TYPE maxbt.
    DATA lv_valicat(18)            TYPE c.
    DATA lv_valbrad(18)            TYPE c.
    DATA lv_val_bra_saude(18)      TYPE c.
    DATA lv_valfmuni(18)           TYPE c.
    DATA lv_cont                   TYPE numc2.
    DATA lv_brad(107)              TYPE c.
    DATA lv_icat(107)              TYPE c.
    DATA lv_fmunic(107)            TYPE c.
    DATA lv_brasaude(107)          TYPE c.
    DATA lv_lines                  TYPE numc3.
    DATA lv_lines2                 TYPE numc3.
    DATA lv_del1                   TYPE char1.
    DATA lv_del2                   TYPE char1.
    DATA lv_tabix                  TYPE sy-tabix.

    CONSTANTS: c_cabec(48)           TYPE c VALUE
               `Valor(es) referente(s) Previdencia Complementar:`,
               c_linha1(58)          TYPE c VALUE
                               `BRADESCO VIDA E PREVIDENCIA SA (CNPJ 51.990.695/0001-37): `,
               c_linha2(42)          TYPE c VALUE
                               `ICATU SEGUROS SA CNPJ(42.283.770/0001-39):`,
               c_linha3(24)          TYPE c VALUE
                               'CNPJ: ( 76870450000125 )',
               c_linha4(107)         TYPE c VALUE
                               'Quadro 05 e linha (03.Outros) refere-se ao valor Participação nos lucros e resultados (PLR)',
               c_linha_bra_saude(54) TYPE c VALUE
                               'BRADESCO SAUDE SA CNPJ: ( 92693118000160 ) ANS: 005711',
               c_blanks1(31)         TYPE c VALUE space,
               c_blanks2(47)         TYPE c VALUE space,
               c_blanks3(65)         TYPE c VALUE space,
               c_blanks4(35)         TYPE c VALUE space.

    RANGES: r_lgart FOR t512w-lgart.

    lv_molga = 37.

    CALL FUNCTION 'CU_READ_RGDIR'
      EXPORTING
        persnr          = hcc-pernr
      IMPORTING
        molga           = lv_molga
      TABLES
        in_rgdir        = lt_rgdir
      EXCEPTIONS
        no_record_found = 1
        OTHERS          = 2.

    CALL FUNCTION 'CD_READ_DATE_RANGE_WITH_PAYDT'
      EXPORTING
        begda     = pn-begda
        endda     = pn-endda
      TABLES
        rgdir_in  = lt_rgdir
        rgdir_out = lt_rgdir_res.

    SELECT SINGLE relid
      FROM t500l
      INTO lv_relid
     WHERE molga EQ lv_molga.

    SELECT lgart, aklas
      FROM t512w
    INTO  TABLE @DATA(lt_t512w)
      WHERE molga = '37'
      AND endda   = `99991231`
      ORDER BY lgart ASCENDING.

    DELETE lt_t512w WHERE aklas+12(2) NE `01`.

    r_lgart-option = 'EQ'.
    r_lgart-sign = 'I'.

    LOOP AT lt_t512w ASSIGNING FIELD-SYMBOL(<fs_t512w>).
      r_lgart-low = <fs_t512w>-lgart.
      APPEND r_lgart.
    ENDLOOP.


    CALL FUNCTION 'HR_IMPORT_RGDIR_FROM_PCLX'
      EXPORTING
        employee_number   = hcc-pernr
        cluster_id        = lv_relid
      TABLES
        import_rgdir      = lt_rgdir_res
      EXCEPTIONS
        no_results        = 1
        no_read_authority = 2
        OTHERS            = 3.

    CLEAR: lt_result[].

    CALL FUNCTION 'PYXX_READ_RGDIR_PAYRESULTS'
      EXPORTING
        clusterid               = lv_relid
        employeenumber          = hcc-pernr
        read_only_international = 'X'
      TABLES
        rgdir                   = lt_rgdir_res[]
        imported_periods        = lt_result
      EXCEPTIONS
        import_error            = 1
        OTHERS                  = 2.

    LOOP AT lt_result INTO ls_result.
      ls_inter = ls_result-inter.
      APPEND ls_inter TO lt_inter.
      ls_evp   = ls_result-evp.
      APPEND ls_evp TO lt_evp.
    ENDLOOP.

* Inicio - alteração - RMNI -  CS1064871 - 23.02.2023
    LOOP AT lt_inter INTO ls_inter.
      ls_wpbp = ls_inter-wpbp.
      APPEND ls_wpbp TO lt_wpbp.
    ENDLOOP.
* Final  - alteração - RMNI -  CS1064871 - 23.02.2023

    LOOP AT lt_wpbp INTO ls_wpbp.
      lv_tabix = sy-tabix.
      READ TABLE ls_wpbp[] ASSIGNING FIELD-SYMBOL(<fs_wpbp>) WITH KEY bukrs = pnpbukrs-low.
      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ELSE.
        READ TABLE lt_inter INTO ls_inter INDEX lv_tabix.
        ls_rt = ls_inter-rt.
        APPEND ls_rt TO lt_rt.

        READ TABLE lt_evp INTO ls_evp INDEX lv_tabix.
        ls_evp_aux = ls_evp.
        APPEND ls_evp_aux TO lt_evp_aux.
      ENDIF.
    ENDLOOP.

* Previdencia
    LOOP AT lt_rt[] INTO ls_rt.
      lv_tabix = sy-tabix.
      LOOP AT ls_rt ASSIGNING FIELD-SYMBOL(<fs_rt>)  WHERE lgart IN r_lgart.
*      READ TABLE ls_rt[] ASSIGNING FIELD-SYMBOL(<fs_rt>) WITH KEY lgart = 'BE31'.
*      IF sy-subrc = 0.
        READ TABLE lt_evp_aux ASSIGNING FIELD-SYMBOL(<fs_evp>) INDEX lv_tabix .
        IF sy-subrc IS INITIAL.
          IF <fs_evp>-paydt+4(2) > 6 .
            lv_icatu = lv_icatu + <fs_rt>-betrg.
          ENDIF.
          IF <fs_evp>-paydt+4(2) <= 6.
            lv_bradesco = lv_bradesco + <fs_rt>-betrg.
          ENDIF.
        ENDIF.
*      ENDIF.
      ENDLOOP.
    ENDLOOP.

* Fundo municipal
    LOOP AT lt_rt[] INTO ls_rt.
      lv_tabix = sy-tabix.
      READ TABLE ls_rt[] ASSIGNING FIELD-SYMBOL(<fs_rt1>) WITH KEY lgart = '3014'.
      IF sy-subrc IS INITIAL.
        lv_fmuni = lv_fmuni + <fs_rt1>-betrg.
      ENDIF.
    ENDLOOP.

* Bradesco Saúde - CBRAND
    LOOP AT lt_rt[] INTO ls_rt.
      READ TABLE ls_rt[] ASSIGNING FIELD-SYMBOL(<fs_rt2>) WITH KEY lgart = 'BE10'.
      IF sy-subrc IS INITIAL.
        lv_bra_saude = lv_bra_saude + <fs_rt2>-betrg.
      ENDIF.
    ENDLOOP.
* Brandesco Saúde Fim - CBRAND


* Previdencia Icatu
    lv_valicat = lv_icatu.
    TRANSLATE lv_valicat USING '.,' .
    CONCATENATE c_linha2
                c_blanks2
                lv_valicat
           INTO lv_icat RESPECTING BLANKS.

* Previdencia Bradesco
    lv_valbrad = lv_bradesco.
    TRANSLATE lv_valbrad USING '.,' .
    CONCATENATE c_linha1
                c_blanks1
                lv_valbrad
           INTO lv_brad RESPECTING BLANKS.


    lv_valfmuni = lv_fmuni.
    TRANSLATE lv_valfmuni USING '.,' .
    CONCATENATE c_linha3
                c_blanks3
                lv_valfmuni
           INTO lv_fmunic RESPECTING BLANKS.

* SAUDE BRADESCO
    lv_val_bra_saude = lv_bra_saude.
    TRANSLATE lv_val_bra_saude USING '.,' .

    CONCATENATE c_linha_bra_saude
                c_blanks4
                lv_val_bra_saude
      INTO lv_brasaude RESPECTING BLANKS.


  ENDIF.

* BEG - RSI - RS22SAP-BR-P01 - T&R-Informe 2022
  MOVE ls_dcc-campo5_08         TO cs_data-campo5_08.
  MOVE ls_dcc-campo5_09         TO cs_data-campo5_09.
  MOVE ls_dcc-campo5_09_titulo1 TO cs_data-campo5_09_tit.
* END - RSI - RS22SAP-BR-P01 - T&R Informe 2022
* Final  - alteração - RMNI -  CSTASK0012612 - 25.01.2023
ENDENHANCEMENT.
