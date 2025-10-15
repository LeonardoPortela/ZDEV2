*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 16/09/2025                                              &*
*& Descrição: Exit Register Data ZFOPEXAGROP                          &*
*& Transação:                                                         &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2SPO |16/09/2025 |Desenvolvimento Inicial.       &*
*&--------------------------------------------------------------------&*
REPORT zrd_zfopexagrop_exit.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZFOPEXAGROP_0001
*&---------------------------------------------------------------------*
*& Prepara a linha de registro ao criar uma nova linha.
*&---------------------------------------------------------------------*
FORM f_exit_zfopexagrop_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfopexagrop TYPE zfopexagrop.

  CLEAR: wl_zfopexagrop.

  wl_zfopexagrop-dt_registro = sy-datum.
  wl_zfopexagrop-hr_registro = sy-uzeit.
  wl_zfopexagrop-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zfopexagrop TO p_registro_manter.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZFOPEXAGROP_0002
*&---------------------------------------------------------------------*
*& Valida dados antes de validar duplicidade de dados e salvar
*&---------------------------------------------------------------------*
FORM f_exit_zfopexagrop_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zfopexagrop TYPE zfopexagrop.

  CLEAR: wl_zfopexagrop.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfopexagrop.

  CLEAR: p_error.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZFOPEXAGROP_0003
*&---------------------------------------------------------------------*
*& Valida dados depois de validar duplicidade de dados e antes de salvar
*&---------------------------------------------------------------------*
FORM f_exit_zfopexagrop_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zfopexagrop TYPE zfopexagrop.

  CLEAR: wl_zfopexagrop.

  MOVE-CORRESPONDING p_registro_manter TO wl_zfopexagrop.

  wl_zfopexagrop-dt_registro = sy-datum.
  wl_zfopexagrop-hr_registro = sy-uzeit.
  wl_zfopexagrop-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zfopexagrop TO p_registro_manter.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZFOPEXAGROP_0004
*&---------------------------------------------------------------------*
*& Carrega a estrutura da tabela de saída (exibição).
*&---------------------------------------------------------------------*
FORM f_exit_zfopexagrop_0004 CHANGING p_saida TYPE any.

  DATA: wl_zfopexagrop_out TYPE ZFOPEXAGROP_out.

  CLEAR: wl_zfopexagrop_out.

  MOVE-CORRESPONDING p_saida TO wl_zfopexagrop_out.

  MOVE-CORRESPONDING wl_zfopexagrop_out TO p_saida.

ENDFORM.
