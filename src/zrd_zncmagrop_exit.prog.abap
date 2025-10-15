*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 16/09/2025                                              &*
*& Descrição: Exit Register Data ZNCMAGROP                            &*
*& Transação:                                                         &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2SPO |16/09/2025 |Desenvolvimento Inicial.       &*
*&--------------------------------------------------------------------&*
REPORT zrd_zncmagrop_exit.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZNCMAGROP_0001
*&---------------------------------------------------------------------*
*& Prepara a linha de registro ao criar uma nova linha.
*&---------------------------------------------------------------------*
FORM f_exit_zncmagrop_0001 CHANGING p_registro_manter TYPE any.

  DATA: wl_zncmagrop TYPE zncmagrop.

  CLEAR: wl_zncmagrop.

  wl_zncmagrop-dt_registro = sy-datum.
  wl_zncmagrop-hr_registro = sy-uzeit.
  wl_zncmagrop-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zncmagrop TO p_registro_manter.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZNCMAGROP_0002
*&---------------------------------------------------------------------*
*& Valida dados antes de validar duplicidade de dados e salvar
*&---------------------------------------------------------------------*
FORM f_exit_zncmagrop_0002 USING p_registro_manter TYPE any
                       CHANGING p_error.

  DATA: wl_zncmagrop TYPE zncmagrop.

  CLEAR: wl_zncmagrop.

  MOVE-CORRESPONDING p_registro_manter TO wl_zncmagrop.

  CLEAR: p_error.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZNCMAGROP_0003
*&---------------------------------------------------------------------*
*& Valida dados depois de validar duplicidade de dados e antes de salvar
*&---------------------------------------------------------------------*
FORM f_exit_zncmagrop_0003 CHANGING p_registro_manter TYPE any.

  DATA: wl_zncmagrop TYPE zncmagrop.

  CLEAR: wl_zncmagrop.

  MOVE-CORRESPONDING p_registro_manter TO wl_zncmagrop.

  wl_zncmagrop-dt_registro = sy-datum.
  wl_zncmagrop-hr_registro = sy-uzeit.
  wl_zncmagrop-us_registro = sy-uname.

  MOVE-CORRESPONDING wl_zncmagrop TO p_registro_manter.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_EXIT_ZNCMAGROP_0004
*&---------------------------------------------------------------------*
*& Carrega a estrutura da tabela de saída (exibição).
*&---------------------------------------------------------------------*
FORM f_exit_zncmagrop_0004 CHANGING p_saida TYPE any.

  DATA: wl_zncmagrop_out TYPE zncmagrop_out.

  CLEAR: wl_zncmagrop_out.

  MOVE-CORRESPONDING p_saida TO wl_zncmagrop_out.

  MOVE-CORRESPONDING wl_zncmagrop_out TO p_saida.

ENDFORM.
