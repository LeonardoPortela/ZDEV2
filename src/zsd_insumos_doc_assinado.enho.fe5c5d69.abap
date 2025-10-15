"Name: \TY:ZCL_INTEGRACAO_BRY_ADIGITAL_IN\IN:ZIF_INTEGRACAO_INJECT\ME:SET_INTEGRAR_INBOUND\SE:BEGIN\EI
ENHANCEMENT 0 ZSD_INSUMOS_DOC_ASSINADO.
*
*-CS2019001753-20.06.2023-#103050-JT-inicio
*-----------------------------
* Obter PDF Assinado Insumos
*-----------------------------
  TRY .
      zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
         )->get_obter_pdf_assinado( i_data = zif_integracao_bry_adigital_in~at_coleta_rec ).

    CATCH zcx_integracao INTO DATA(ex_integra).
    CATCH zcx_error INTO DATA(ex_error).
  ENDTRY.
*-CS2019001753-20.06.2023-#103050-JT-fim
*
ENDENHANCEMENT.
