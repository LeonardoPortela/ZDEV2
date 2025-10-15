*--------------------------------------------------------------------*
*                         Consultoria                                *
*--------------------------------------------------------------------*
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 09.02.2023                                              *
* Descrição: Bloqueio/Validação VF01 campo Class. Contab. Material   *
* Report   : ZSDR0149                                                *
*--------------------------------------------------------------------*
* Projeto  : CS2023000060
*--------------------------------------------------------------------*

DATA: l_mesg1 TYPE char50,
      l_mesg2 TYPE char50,
      l_mesg3 TYPE char50.

IF sy-tcode = 'VF01' OR
   sy-tcode = 'ZLES0136'.

  SELECT SINGLE vgbel, vgpos
    INTO @DATA(w_lips)
    FROM lips
   WHERE vbeln = @vbrp-vgbel
     AND posnr = @vbrp-vgpos.

  IF sy-subrc = 0.
    SELECT SINGLE vbeln, posnr, ktgrm
      INTO @DATA(w_vbap)
      FROM vbap
     WHERE vbeln = @w_lips-vgbel
       AND posnr = @w_lips-vgpos.

    IF sy-subrc = 0 AND w_vbap-ktgrm IS INITIAL.
*     l_mesg1 = | { 'Item: ' } { w_vbap-posnr } { 'não possui parâmetro de ' } |.
*     l_mesg2 = | { 'Classificação Contábil de Material. ' } | .
*     l_mesg3 = | { 'Por favor criar FI para ajuste.' } | .
*
*     PERFORM vbfs_hinzufuegen_allg USING vbrp-vgbel vbrp-vgpos  'SD' 'E' '024'
*                                         l_mesg1    l_mesg2     l_mesg3 space.
*     READ TABLE xvbrk INDEX 1.
*     IF sy-subrc = 0.
*       DELETE xvbrk INDEX 1.
*     ENDIF.
*     FREE: xvbrk, xvbrp, vbrk, vbrp.

      MESSAGE e024(sd) WITH 'Item: ' w_vbap-posnr 'não possui parâmetro de Classificação Contábil'
                            ' de Material. Abrir no SE uma FI para CSC Fiscal!'.
    ENDIF.
  ENDIF.
ENDIF.

*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
