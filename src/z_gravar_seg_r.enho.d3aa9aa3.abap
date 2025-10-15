"Name: \PR:RFFOBR_D\FO:STORE_ON_FILE\SE:END\EI
ENHANCEMENT 0 Z_GRAVAR_SEG_R.
*Lógica para gravar o Seguimento R

  DATA: wl_zfi_j_1bdmexr TYPE zfi_j_1bdmexr,
        wl_zfi_j_1bdmeym TYPE zfi_j_1bdmeym,
        wl_j_1bdmexq     TYPE j_1bdmexq,
        wl_j_1bdmexh1    TYPE j_1bdmexh1,
        vl_q05           TYPE j_1bdmexq-q05,
        vqtde            TYPE i.


  FIELD-SYMBOLS: <fs_zfi_j_1bdmexr> TYPE zfi_j_1bdmexr,
                 <fs_zfi_j_1bdmeym> TYPE zfi_j_1bdmeym,
                 <fs_j_1bdmexh1>    TYPE j_1bdmexh1,
                 <fs_q05>           TYPE j_1bdmexq-q05.
  FIELD-SYMBOLS: <component> TYPE any.


  CLEAR vqtde .
  DO.
    ASSIGN COMPONENT sy-index OF STRUCTURE daten TO <component>.
    IF sy-subrc NE 0.
      vqtde = sy-index.
      EXIT.
    ENDIF.
  ENDDO.
***  IF VQTDE eq 22. "Header
*****    j_1bdmexh1
***    ASSIGN ('(RFFOBR_D)J_1BDMEXH1') to <fs_j_1bdmexh1>.
***    IF <fs_j_1bdmexh1>-H101 eq '001'."Banco do Brasil
***      IF <fs_j_1bdmexh1>-h107+13(2) eq '17'.
***        <fs_j_1bdmexh1>-H121+51(03) = 'CSP'.      "Identificação de Cobrança
***        <fs_j_1bdmexh1>-H121+54(03) = '000'.      "Uso exclusivo de Vans
***
***        MOVE-CORRESPONDING <fs_j_1bdmexh1> to wl_j_1bdmexh1.
***          PERFORM cr_lf."Finaliza o header
***          PERFORM store_on_file USING wl_j_1bdmexh1."Inicia a gravaç
***          UNASSIGN <fs_j_1bdmexh1>.
***          CLEAR:wl_j_1bdmexh1.
***      ENDIF.
***    ENDIF.
***  ENDIF.

  IF vqtde EQ 23. "Seguimento Q
    ASSIGN ('(RFFOBR_D)ZFI_J_1BDMEXR') TO <fs_zfi_j_1bdmexr>.
    IF <fs_zfi_j_1bdmexr> IS ASSIGNED.
      MOVE-CORRESPONDING <fs_zfi_j_1bdmexr> TO wl_zfi_j_1bdmexr."Obtem o Seguimento R
      IF wl_zfi_j_1bdmexr-r05 IS NOT INITIAL."Se o seg R existir, então grava
        PERFORM cr_lf."Finaliza o Seg Q
        PERFORM store_on_file USING wl_zfi_j_1bdmexr."Inicia a gravação do Seg R
        UNASSIGN <fs_zfi_j_1bdmexr>.
        CLEAR:wl_zfi_j_1bdmexr.
      ENDIF.
    ENDIF.
  ENDIF.

  IF vqtde EQ 43. "Grava multa Itau
    ASSIGN ('(RFFOBR_D)ZFI_J_1BDMEYM') TO <fs_zfi_j_1bdmeym>.
    IF <fs_zfi_j_1bdmeym> IS ASSIGNED.
      MOVE-CORRESPONDING <fs_zfi_j_1bdmeym> TO wl_zfi_j_1bdmeym."Obtem multa Itau
      IF wl_zfi_j_1bdmeym-m01 IS NOT INITIAL."Se multa Itau existir, então grava
        PERFORM cr_lf."Finaliza o IDS ITAÚ - detalhes
        PERFORM store_on_file USING wl_zfi_j_1bdmeym."Inicia a gravação multa Itau
        UNASSIGN <fs_zfi_j_1bdmeym>.
        CLEAR:wl_zfi_j_1bdmeym.
      ENDIF.
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
