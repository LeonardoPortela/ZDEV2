*&--------------------------------------------------------------------&*
*&                     Relatório Módulo - SD                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Ronaldo Freitas                                         &*
*& Data.....: 02/09/2024                                              &*
*& Descrição: Contabilização e Cálculo PIS\COFINS Presumido           &*
*& Transação: JOB                                                     &*
*&--------------------------------------------------------------------&*
*& Projeto  : Ninjas Evolution                                        &*
*& Código Espec.Funcional/Técnica: 146775                             &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
*&---------------------------------------------------------------------*
*& Report ZFIS0001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zfis0001.

*---------------------------------------------------------------------*
* Declarações
*---------------------------------------------------------------------*
tables: zsdt0245, vbrk.

data:
  lt_auart   type table of zcl_cont_credito=>ty_auart,
  lt_werks   type table of zcl_cont_credito=>ty_werks,
  lt_matnr   type table of zcl_cont_credito=>ty_matnr,
  lt_matkl   type table of zcl_cont_credito=>ty_matkl,
  lt_vbeln   type table of zcl_cont_credito=>ty_vbeln,
  lt_vkorg   type table of zcl_cont_credito=>ty_vkorg,
  lv_message type string.

*======================================================================*
*& TELA DE SELEÇÃO
*======================================================================*
selection-screen: begin of block b1 with frame title text-001.
  select-options:
                  s_werks  for  zsdt0245-werks, " OBLIGATORY NO INTERVALS NO-EXTENSION,
                  s_matnr  for  zsdt0245-matnr,
                  s_fkart  for  vbrk-fkart,
                  s_vbeln  for  vbrk-vbeln.
selection-screen: end of block b1.

*---------------------------------------------------------------------*
* Lógica Principal
*---------------------------------------------------------------------*

try.
**---------------------------------------------------------------------*
** Busca informações relevantes para o processo
**---------------------------------------------------------------------*

    data(go) = new zcl_cont_credito( ).

    if sy-batch is initial.
      if s_werks[] is not initial
      or s_matnr[] is not initial
      or s_fkart[] is not initial
      or s_vbeln[] is not initial.

        lt_werks = s_werks[].
        lt_matnr = s_matnr[].
        lt_auart = s_fkart[].
        lt_vbeln = s_vbeln[].

        if s_matnr is not initial.
          select single matkl from mara into @data(va_matkl) where matnr eq @s_matnr-low.
          if sy-subrc eq 0.
            lt_matkl = value #( ( sign = 'I' option = 'EQ' low = va_matkl ) ).
          endif.
        endif.

        go->set( lt_werks = lt_werks[]
                 lt_matnr = lt_matnr[]
                 lt_auart = lt_auart[]
                 lt_vbeln = lt_vbeln[]
                 lt_matkl = lt_matkl[] ).
      else.
        message 'Parametro de seleção obrigatório!'(e04) type 'I' display like 'E'.
        exit.
      endif.
    endif.

    go->process( importing lt_message = lv_message ).

    if sy-batch is initial and lv_message is not initial.
      message lv_message type 'I' display like 'E'.
      exit.
    endif.

  catch cx_root.
    if sy-batch is initial.
      message 'Erro no processamento!'(e01) type 'E'.
    endif.
endtry.
