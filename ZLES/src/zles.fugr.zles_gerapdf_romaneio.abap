FUNCTION zles_gerapdf_romaneio.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_CHAVE) TYPE  ZCH_REF
*"     REFERENCE(I_EMAIL) TYPE  AD_SMTPADR OPTIONAL
*"----------------------------------------------------------------------

**** gerar pdf com danfe - Impressão Nota Fiscal
*  PERFORM f_call_danfe_dacte IN PROGRAM zlesr0102_form USING '1' IF FOUND.
*
**** gerar pdf com CTE/MDFE e Documentos de Transporte
*  PERFORM f_call_danfe_dacte IN PROGRAM zlesr0102_form USING '2' IF FOUND.

*** gerar pdf com Contrato de Frete e Pedágio

*** fazer download dos arquivos pdf ou enviar e-mail.


ENDFUNCTION.
