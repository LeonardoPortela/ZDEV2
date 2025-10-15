FUNCTION z_fi_outbound_lote_compra_add.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RETURN STRUCTURE  ZGL002_COMP_F44
*"----------------------------------------------------------------------


  TYPES: BEGIN OF ty_bkpf_gen,
           belnr TYPE bkpf-belnr,
           bukrs TYPE bkpf-bukrs,
           gjahr TYPE bkpf-gjahr,
           budat TYPE bkpf-budat,
           kurs2 TYPE bkpf-kurs2,
           cpudt TYPE bkpf-cpudt,
           usnam TYPE bkpf-usnam,
         END OF ty_bkpf_gen.

  TYPES: BEGIN OF ty_bkpf_return,
           belnr TYPE bkpf-belnr,
           bukrs TYPE bkpf-bukrs,
         END OF ty_bkpf_return.

  DATA: lwa_dados_add      TYPE zsfi0002,
*--> 22.05.2023 - Migration S4 – MIGNOW - Start
*        lva_json_dados_add TYPE char30000,
        lva_json_dados_add TYPE string,
*<-- 22.05.2023 - Migration S4 – MIGNOW – End
        lwa_bkpf_return    TYPE ty_bkpf_return,
        lit_bkpf_return    TYPE TABLE OF ty_bkpf_return,
        lit_bkpf           TYPE TABLE OF ty_bkpf_gen.

  LOOP AT return ASSIGNING FIELD-SYMBOL(<fs_return>).

    CHECK strlen( <fs_return>-sgtxt ) >= 20.

    APPEND INITIAL LINE TO lit_bkpf_return ASSIGNING FIELD-SYMBOL(<fs_bkpf_return>).

    <fs_bkpf_return>-bukrs = <fs_return>-bukrs.
    <fs_bkpf_return>-belnr = <fs_return>-sgtxt+10(10).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_bkpf_return>-belnr
      IMPORTING
        output = <fs_bkpf_return>-belnr.

  ENDLOOP.

  IF lit_bkpf_return[] IS NOT INITIAL.


    SELECT belnr bukrs gjahr budat kurs2 cpudt usnam
      FROM bkpf INTO TABLE lit_bkpf
      FOR ALL ENTRIES IN lit_bkpf_return
      WHERE belnr = lit_bkpf_return-belnr
        AND bukrs = lit_bkpf_return-bukrs.

    LOOP AT return ASSIGNING <fs_return>.

      CHECK strlen( <fs_return>-sgtxt ) >= 20.

      lwa_bkpf_return-bukrs = <fs_return>-bukrs.
      lwa_bkpf_return-belnr = <fs_return>-sgtxt+10(10).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_bkpf_return-belnr
        IMPORTING
          output = lwa_bkpf_return-belnr.

      READ TABLE lit_bkpf ASSIGNING FIELD-SYMBOL(<fs_bkpf>) WITH KEY bukrs = lwa_bkpf_return-bukrs
                                                                     belnr = lwa_bkpf_return-belnr.

      IF sy-subrc EQ 0.
        CLEAR : lwa_dados_add.
        MOVE-CORRESPONDING <fs_bkpf> TO lwa_dados_add.

        lva_json_dados_add  = /ui2/cl_json=>serialize( EXPORTING data = lwa_dados_add ).

        DATA(_len) = strlen( lva_json_dados_add ).
        IF _len LT 1300.
          <fs_return>-info_adicional_1 = lva_json_dados_add+0000(_len).
        ELSE.
          <fs_return>-info_adicional_1 = lva_json_dados_add+0000(1300).
          _len = _len - 1300.
          IF _len LT 1300.
            <fs_return>-info_adicional_2 = lva_json_dados_add+1300(_len).
          ELSE.
            <fs_return>-info_adicional_2 = lva_json_dados_add+1300(1300).
            _len = _len - 1300.
            IF _len LT 1300.
              <fs_return>-info_adicional_3 = lva_json_dados_add+2600(_len).
            ELSE.
              <fs_return>-info_adicional_3 = lva_json_dados_add+2600(1300).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

*--> 24.08.2023 18:11:25 - Migração S4 – ML - Início
*  CALL FUNCTION 'Z_FI_OUTBOUND_LOTE_COMPRA' IN BACKGROUND TASK
*    DESTINATION 'XI_SIGAM_LOTE_COMPRA'
*    AS SEPARATE UNIT
*    TABLES
*      return = return.

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_LOTE_COMPRA'.

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
        return = return.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      TABLES
        return = return.
  ENDIF.

  COMMIT WORK.

*<-- 24.08.2023 18:11:25 - Migração S4 – ML – Fim
ENDFUNCTION.
