*----------------------------------------------------------------------*
***INCLUDE LZXML_SIMETRYAF05 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ADD
*&---------------------------------------------------------------------*
*       Add código HTML - Truncar
*----------------------------------------------------------------------*

FORM add  TABLES   p_html  STRUCTURE w3html
          USING    p_texto TYPE string.
  CALL FUNCTION 'ZHTML_ADD'
    EXPORTING
      i_texto = p_texto
    TABLES
      it_html = p_html.
ENDFORM.                    " ADD

*&---------------------------------------------------------------------*
*&      Form  MONTA_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_HTML  text
*----------------------------------------------------------------------*
FORM monta_cabecalho  TABLES p_html  STRUCTURE w3html.

  DATA: vg_html           TYPE string.

  "Início cabeçalho
  PERFORM add TABLES p_html USING '<tr><td>'.
  PERFORM add TABLES p_html USING '<BR>'.
  PERFORM add TABLES p_html USING '<DIV align=center><FONT face=Arial color=#ff0000 size=4><STRONG>Doc. Eletrônicos (NF-e/CT-e)</STRONG></FONT></DIV>'.
  PERFORM add TABLES p_html USING '<DIV align=left>'.
  PERFORM add TABLES p_html USING '<FONT face=Arial color=#0000ff size=2>'.

  "Início da Tabela de detalhe
  PERFORM add TABLES p_html USING '<table align="left" cellspacing="0" cellpadding="9" border="1">'.
  CONCATENATE '<tr><td colspan="9" align="center" bgcolor="666666"><font color="#FFFFFF" size=2><strong>'
              'Relação de Doc. Eletrônicos'
              '</strong></font></td></tr>'
         INTO vg_html.
  PERFORM add TABLES p_html USING vg_html.
  CONCATENATE '<tr>'
              '<td><font color="#000030" size=2><b>Docnum</b></font></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Empresa</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Filial</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Modelo</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Número</b></font></p></td>'
              '<td><p align="right"><font color="#000030" size=2><b>Série</b></font></p></td>'
              '<td><font color="#000030" size=2><b>Dt. Emissão</b></font></td>'
              '<td><font color="#000030" size=2><b>Usuário</b></font></td>'
              '<td><font color="#000030" size=2><b>Etapa Necessária</b></font></td>'
              '</tr>' INTO vg_html.
  PERFORM add TABLES p_html USING vg_html.

ENDFORM.                    " MONTA_CABECALHO

*&---------------------------------------------------------------------*
*&      Form  MONTA_DETALHE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_HTML  text
*      -->P_WA_J_1BNFDOC  text
*----------------------------------------------------------------------*
FORM monta_detalhe  TABLES p_html  STRUCTURE w3html
                     USING wa_j_1bnfdoc      TYPE j_1bnfdoc
                           wa_j_1bnfe_active TYPE j_1bnfe_active
                           wa_dmo_text       TYPE dd07v.

  DATA: vg_html TYPE string,
        xmodelo TYPE c LENGTH 02,
        xnumero TYPE c LENGTH 09,
        xserie  TYPE c LENGTH 03.

  WRITE: wa_j_1bnfdoc-model  TO xmodelo,
         wa_j_1bnfdoc-nfenum TO xnumero,
         wa_j_1bnfdoc-series TO xserie .

  SHIFT:  xmodelo LEFT DELETING LEADING space,
          xnumero LEFT DELETING LEADING space,
          xserie  LEFT DELETING LEADING space.

  CONCATENATE '<tr>'
              '<td><font color="#000000" size=2>' wa_j_1bnfdoc-docnum '</font></td>'
              '<td><font color="#000000" size=2>' wa_j_1bnfdoc-bukrs  '</font></td>'
              '<td><font color="#000000" size=2>' wa_j_1bnfdoc-branch '</font></td>'
              '<td><p align="right"><font color="#000000" size=2>' xmodelo '</font></p></td>'
              '<td><p align="right"><font color="#000000" size=2>' xnumero '</font></p></td>'
              '<td><p align="right"><font color="#000000" size=2>' xserie  '</font></p></td>'
              '<td><font color="#000000" size=2>' wa_j_1bnfdoc-docdat+6(2) '/' wa_j_1bnfdoc-docdat+4(2) '/' wa_j_1bnfdoc-docdat(4) '</font></td>'
              '<td><font color="#000000" size=2>' wa_j_1bnfdoc-crenam '</font></td>'
              '<td><font color="#000000" size=2>' wa_dmo_text-ddtext '</font></td>'
              '</tr>' INTO vg_html.
  PERFORM add TABLES p_html USING vg_html.


ENDFORM.                    " MONTA_DETALHE

*&---------------------------------------------------------------------*
*&      Form  MONTA_FIM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_HTML  text
*      -->P_PACKING_LIST  text
*      -->P_DOCUMENT_DATA  text
*----------------------------------------------------------------------*
FORM monta_fim  TABLES   p_html       STRUCTURE w3html
                         packing_list STRUCTURE sopcklsti1
                USING    document_data TYPE sodocchgi1.

  "Fim da Tabela de detalhe
  PERFORM add TABLES p_html USING '</table>'.
  PERFORM add TABLES p_html USING '</DIV>'.
  PERFORM add TABLES p_html USING '</td></tr>'.

  "Fim cabeçalho
  PERFORM add TABLES p_html USING '</DIV>'.
  PERFORM add TABLES p_html USING '</td></tr>'.

  document_data-obj_name  = 'ProcArqC'.
  document_data-obj_descr = 'Documentos Eletrônicos NF-e/CT-e'.
  document_data-no_change = 'X'.

  packing_list-head_start = 1.
  packing_list-head_num   = 60.
  packing_list-body_start = 1.
  packing_list-body_num   = 99999.
  packing_list-doc_type   = 'HTM'.
  APPEND packing_list.


ENDFORM.                    " MONTA_FIM
