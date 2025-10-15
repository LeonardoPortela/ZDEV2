*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Vitor Rienzo                                            &*
*& Data.....: 11.06.2024                                              &*
*& Descrição: Registro de modificação de preço em massa               &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&                                                                &*

REPORT zmmr200.

INCLUDE zmmr200_ssc.

INITIALIZATION.

  DATA(lo_control) = NEW zcl_mm_mr21_mr22( ).

START-OF-SELECTION.

  lo_control->execute( iv_bukrs = p_bukrs
                       iv_data  = p_data
                       iv_ref   = p_ref
                       ib_mr21  = rb_mr21
                       ib_mr22  = rb_mr22 ).

INCLUDE zmmr200_status_0200o01.

INCLUDE zmmr200_user_command_0200i01.
