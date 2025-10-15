*---------------------------------------------------------------------*
* Enhacement. : ZSD_ENHA_TXT_NF_IDIOMA
* Programa... : LJ1BBF40
* Include.... : ZSDI_TXT_IMP_NF_IDIOMA
* Finalidade. : Preenchimento automático do campo SPRAS_BUPLA
*               com base em regras configuradas na TVARVC.
*---------------------------------------------------------------------*
* Chamado.... : IR238394
* Autor...... : LAZAROSR
* Consultoria : Stefanini
* Data....... : 29/05/2025
*---------------------------------------------------------------------*
* Notas...... :
* - Verifica se o programa e o tipo de operação DIRECT estão
*   habilitados para preenchimento do idioma específico via classe
*   ZCL_TEXTO_IMPOSTO_NF_IDIOMA.
* - Regras controladas via TVARVC:
*     -> ZSD_TXT_NF_IMPOSTO_PROGRAMA
*     -> ZSD_TXT_NF_IMPOSTO_DIRECT
*---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include ZSDI_TXT_IMP_NF_IDIOMA
*&---------------------------------------------------------------------*
TRY.

    DATA(lo_validador) = NEW zcl_texto_imposto_nf_idioma( i_w_j_1bnfdoc = wnfdoc ).

    CALL METHOD lo_validador->atribuir_idioma
      CHANGING
        c_w_j_1bnfdoc = wnfdoc.

  CATCH cx_root.
    " Erro ao atribuir o valor...
ENDTRY.
