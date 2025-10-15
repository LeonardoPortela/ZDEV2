FUNCTION zsd_importar_dados_takeup.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"  EXPORTING
*"     REFERENCE(E_IMPORTOU) TYPE  CHAR1
*"----------------------------------------------------------------------

  FREE: g_file_name,
        g_importou.

  l_bukrs = i_bukrs.

*----------------------------------------
* carregar / validar / salvar arquivo
*----------------------------------------
  CALL SCREEN 100 STARTING AT 40  5
                    ENDING AT 132 6.

  e_importou = g_importou.

ENDFUNCTION.
