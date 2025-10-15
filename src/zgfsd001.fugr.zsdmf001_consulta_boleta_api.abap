FUNCTION zsdmf001_consulta_boleta_api.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_DOC_SIMU) TYPE  ZDOC_SIMU OPTIONAL
*"     REFERENCE(IS_REQUEST) TYPE  ZSDE0026
*"  EXPORTING
*"     REFERENCE(ET_BOLETAS) TYPE  ZSDC013
*"----------------------------------------------------------------------

  DATA lw_dados TYPE zsde0020.

  CLEAR et_boletas[].

  CHECK is_request IS NOT INITIAL.

  PERFORM f_data_sigam USING is_request-bldat CHANGING lw_dados-dtvencimento.

  PERFORM f_formata_bukrs_boleta USING is_request-bukrs CHANGING lw_dados-codempresa.

  TRY.

      "APPEND 'RV' TO lw_dados-tpboletalista.
      APPEND 'C' TO lw_dados-tpboletalista.

      DATA(lt_dados) = zcl_int_sigam_hedge_consultar=>zif_int_sigam_consultar_hedge~get_instance( )->enviar_sigam( lw_dados ).

      LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE idobrigacao = '8'.

        APPEND INITIAL LINE TO et_boletas ASSIGNING FIELD-SYMBOL(<fs_boletas>).

        <fs_boletas>-doc_simu = iv_doc_simu.
        <fs_boletas>-tp_obri = <fs_dados>-idobrigacao.
        "<fs_boletas>-ident = lines( et_boletas ).

        "UNPACK <fs_boletas>-ident to <fs_boletas>-ident.
        "<fs_boletas>-ident = <fs_boletas>-ident+10.

        <fs_boletas>-ident = <fs_dados>-identificacao.

        <fs_boletas>-data_venc = <fs_dados>-dtvencimento.
        "<fs_boletas>-data_venc = is_request-bldat. "18.01.2023

        IF <fs_dados>-vrreal < 0.
          <fs_boletas>-vlr_brl = <fs_dados>-vrreal * -1.
        ELSE.
          <fs_boletas>-vlr_brl = <fs_dados>-vrreal.
        ENDIF.

        <fs_boletas>-taxa_neg = <fs_dados>-txdolarnegociado.

        IF <fs_dados>-vrdolarnegociado < 0.
          <fs_boletas>-vlr_dolar_neg = <fs_dados>-vrdolarnegociado * -1.
        ELSE.
          <fs_boletas>-vlr_dolar_neg = <fs_dados>-vrdolarnegociado.
        ENDIF.

        <fs_boletas>-taxa_curva = <fs_dados>-txdolarcurva.

        IF <fs_dados>-vrdolarcurva < 0.
          <fs_boletas>-vlr_dolar_proj = <fs_dados>-vrdolarcurva * -1.
        ELSE.
          <fs_boletas>-vlr_dolar_proj = <fs_dados>-vrdolarcurva.
        ENDIF.

        "<fs_boletas>-IDENT_REV_SIGAM = <fs_dados>-
        <fs_boletas>-coment = <fs_dados>-comentarios.
        <fs_boletas>-saldo = <fs_dados>-saldo.

      ENDLOOP.

    CATCH zcx_integracao .
    CATCH zcx_error .
  ENDTRY.

  SORT et_boletas BY data_venc ASCENDING.


ENDFUNCTION.
