FUNCTION zmm_converte_eban_coupa_interm.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_EBAN) TYPE  ZMMC_DADOS_INT_COUPA_EBAN
*"  EXPORTING
*"     REFERENCE(ET_INTERMED_TAB) TYPE  ZMMC_DADOS_INT_COUPA_INTERMEDI
*"----------------------------------------------------------------------

  DATA lv_count TYPE i.

  CHECK it_eban[] IS NOT INITIAL.

  LOOP AT it_eban ASSIGNING FIELD-SYMBOL(<fs_eban>).

    " preenche cabeçalho
    IF sy-tabix = 1.

      APPEND INITIAL LINE TO et_intermed_tab ASSIGNING FIELD-SYMBOL(<fs_inter>).

      PERFORM f_preenche_cab
        USING <fs_eban>
        CHANGING <fs_inter>.

    ENDIF.

    ADD 1 TO lv_count.

    PERFORM f_preenche_item
      USING lv_count
   CHANGING <fs_inter>
            <fs_eban>.

  ENDLOOP.

ENDFUNCTION.
