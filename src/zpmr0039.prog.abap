*&---------------------------------------------------------------------*&
*& Report  ZPMR0039                                                    *&
*&                                                                     *&
*& Descrição....: Integração SAP x ALS (s360)                          *&
*& Analista.....: Cleudo Ferreira                                      *&
*& Desenvolvedor: Anderson Oenning.                                    *&
*& Data 01/01/2019.                                                    *&
*&                                                                     *&
*& Modulo.......: PM                                                   *&
*&---------------------------------------------------------------------*&

* Programa ZPMR0039 Executado pelo JOB SAP_ALS, para listar todas as amostras e guardar no banco ZTPM_L_AMOST.

REPORT ZPMR0039.

INCLUDE ZLCLASS.

START-OF-SELECTION.

  DATA: OBJ_INST TYPE REF TO ZCL_ZPM_GERAR_TOKEN.
  CREATE OBJECT OBJ_INST.

*&------------------------------------------------------------------------------*
* Coletando token de acesso.                                                    *
* View https://homol.s360.com.br/api/login                                      *
*&------------------------------------------------------------------------------*
  CALL METHOD OBJ_INST->ZGERAR_TOKEN
    EXPORTING
      TOKEN = OBJ_INST->LC_TOKEN-ACCESS_TOKEN.

*&------------------------------------------------------------------------------*
* Coletando todas as amostras                                                   *
* https://homol.s360.com.br/api/v1/amostra/list?token_type=Bearer&access_token= *
*&------------------------------------------------------------------------------*

  CHECK OBJ_INST->LC_TOKEN-ACCESS_TOKEN IS NOT INITIAL.
  CALL METHOD OBJ_INST->ZGERAR_LIST_AMOSTRA
    IMPORTING
      TOKEN = OBJ_INST->LC_TOKEN-ACCESS_TOKEN.

*&----------------------------------------------------------------------*
* Coletando o resultado das amostras por empresa                        *                                    *
* https://homol.s360.com.br/api/v3/resultadoAmostra/view?numeroAmostra= *
*&----------------------------------------------------------------------*
  CALL METHOD OBJ_INST->ZGERAR_RES_AMOSTRA
    IMPORTING
      TOKEN = OBJ_INST->LC_TOKEN-ACCESS_TOKEN.
