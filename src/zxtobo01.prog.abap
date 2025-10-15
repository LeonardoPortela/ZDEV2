*----------------------------------------------------------------------*
***INCLUDE ZXTOBO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 24/09/2024  |DEVK9A2791  |NSEGANTIN      |Eqpt Tp 01, Obrig Imobiliz*
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_fields_2000 OUTPUT.
  IF sy-tcode = 'IE03'.
    LOOP AT SCREEN.
      CHECK screen-group1 = 'GR1'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
**<<<------"149356 - NMS - INI------>>>
* Verifica se é criação ou modificação de equipamento.
  CHECK 'IE01_IE02' CS sy-tcode.
  LOOP AT SCREEN.
* Verifica se o Grupo 1 é GR1 e o 2 é IMB (Imobilizado Agro) do campo.
    CHECK screen-group1 = 'GR1' AND
          screen-group2 = 'OBL'.
* Carrega a TI ITOB da área de memória.
    ASSIGN ('(SAPLITO0)ITOB') TO FIELD-SYMBOL(<fs_itob>).
* Valida a caraga da área de memória.
    IF <fs_itob> IS ASSIGNED.
      ASSIGN COMPONENT 'EQTYP' OF STRUCTURE <fs_itob> TO FIELD-SYMBOL(<fs_eqtyp>).
* Valida a caraga do campo da estrutura.
      IF <fs_eqtyp> IS ASSIGNED.
* Verifica o tipo de equipamento.
        CASE <fs_eqtyp>.
          WHEN '1'.   "1-Agro Próprio
            screen-required = 2. "Marca com sinal de obrogatório sem validação standard

          WHEN '2' OR "2-Agro Locado
               '3' OR "3-Agro Terceiro
               '4'.   "4-Agro Emprestado/Teste
            screen-input    = 0. "Fecha o campo para edição
          WHEN OTHERS.
*         Do nothing
        ENDCASE.

        UNASSIGN: <fs_itob>, <fs_eqtyp>.

      ENDIF.

    ELSE.
      UNASSIGN <fs_itob>.

    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.
**<<<------"149356 - NMS - FIM------>>>
ENDMODULE.
