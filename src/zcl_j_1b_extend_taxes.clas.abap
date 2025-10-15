CLASS zcl_j_1b_extend_taxes DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_badi_j1b_extend_taxes .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_J_1B_EXTEND_TAXES IMPLEMENTATION.


  METHOD if_ex_badi_j1b_extend_taxes~determine_discount_in_ipi_base.

  ENDMETHOD.


  METHOD if_ex_badi_j1b_extend_taxes~exclude_icms_from_pis_cof_base.

    DATA: w_campo(40),
          w_corte(1),
          v_miro(14). "BAPI

*Begin - 25.05.2023 SD-RMNI-IR138564-CS1096173-Ajuste Excl ICMS HF
    CONSTANTS:
      lc_zsd_exclude_icms_base_p_c TYPE tvarvc-name VALUE 'ZSD_EXCLUDE_ICMS_BASE_PIS_COF',
      lc_zsd_exclude_icms_base_tax TYPE tvarvc-name VALUE 'ZSD_EXCLUDE_ICMS_BASE_TAX'.
*End - 25.05.2023 SD-RMNI-IR138564-CS1096173-Ajuste Excl ICMS HF

    FIELD-SYMBOLS: <fs_rbkpv> TYPE any.
    FIELD-SYMBOLS: <fs_sgtxt> TYPE any.

    CASE is_icms_excl_info-komk-kappl.
      WHEN 'TX'.
        w_corte = 'N'.
        IF sy-tcode = 'MR8M' OR sy-tcode = 'MBST'.
          w_campo = '(SAPLMR1M)RBKPV-BUDAT'.
          ASSIGN (w_campo) TO <fs_rbkpv>. "Documento original MIRO
          IF <fs_rbkpv> IS ASSIGNED.
*          IF <fs_rbkpv> = '00000000'.
*            IMPORT   v_miro to v_miro   FROM MEMORY ID 'MIRO_CANCEL'.
*            SELECT SINGLE budat
*              FROM rbkp into <fs_rbkpv>
*              WHERE belnr = v_miro+0(10)
*              AND   gjahr = v_miro+10(4).
*          ENDIF.
            SELECT SINGLE *
                 FROM tvarvc INTO @DATA(lwa_tvarv)
                 WHERE name = 'ZMM_EXCLUIR_ICMS_DATA_CORTE'
                 AND   low  <> ''.
            IF sy-subrc IS INITIAL AND <fs_rbkpv> <> '00000000'.
              IF <fs_rbkpv> < lwa_tvarv-low. "Data Corte
                w_corte = 'S'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        SELECT COUNT(*)
          FROM tvarvc
          WHERE name = 'ZMM_EXCLUIR_ICMS_BASE_PIS_COF'
          AND   low  = abap_true.
        IF sy-subrc IS INITIAL AND w_corte = 'N'.
          cs_icms_excl_params-icms = abap_true.
        ENDIF.
        IF sy-tcode = 'MIRO'.
          w_campo = '(SAPLMR1M)RBKPV-SGTXT'.
          ASSIGN (w_campo) TO <fs_sgtxt>.
          IF <fs_sgtxt> IS ASSIGNED.
            IF <fs_sgtxt> = 'ANTIGO'.
              cs_icms_excl_params-icms = abap_false.
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN 'V'.
*Begin - 25.05.2023 SD-RMNI-IR138564-CS1096173-Ajuste Excl ICMS HF
*Solução paliativa para cenários SD com exclusão de ICMS
        "Ativa/desativa ajuste
        SELECT SINGLE low FROM tvarvc INTO @DATA(lv_check_active)
          WHERE name = @lc_zsd_exclude_icms_base_p_c.
        IF lv_check_active = abap_true.
          "Confere Código de imposto
          SELECT SINGLE low FROM tvarvc INTO @DATA(lv_cd_tax)
            WHERE name = @lc_zsd_exclude_icms_base_tax
              AND low = @is_icms_excl_info-komp-j_1btxsdc.
          IF sy-subrc = 0.
            "Preenche parâmetro para cenário SD
            cs_icms_excl_params-icms = abap_true.
          ENDIF.
        ENDIF.
*End - 25.05.2023 SD-RMNI-IR138564-CS1096173-Ajuste Excl ICMS HF

      WHEN OTHERS.
    ENDCASE.



  ENDMETHOD.


  METHOD if_ex_badi_j1b_extend_taxes~icms_st_recalculate.

  ENDMETHOD.


  METHOD if_ex_badi_j1b_extend_taxes~partilha_icms_recalculate.
  ENDMETHOD.


  METHOD if_ex_badi_j1b_extend_taxes~subtract_disc_from_ipi_base.
  ENDMETHOD.
ENDCLASS.
