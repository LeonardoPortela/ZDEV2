FUNCTION ZPLANCOMP_EMAIL_EXECUCAO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WA_ZNOM_REMETENTE) TYPE  ZNOM_REMETENTE
*"  TABLES
*"      IT_HTML STRUCTURE  W3HTML
*"----------------------------------------------------------------------

  DATA: VG_NUMNFE              TYPE STRING,
        VG_NUMCHAR             TYPE STRING,
        VG_HTML                TYPE STRING,
        VG_SEPARATE            TYPE C LENGTH 1,
        XNUMERO                TYPE C LENGTH 15,
        WA_ZNOM_REME_NOTAS     TYPE ZNOM_REME_NOTAS,
        IT_J_1BNFDOC           TYPE TABLE OF J_1BNFDOC WITH HEADER LINE,
        IT_J_1BNFLIN           TYPE TABLE OF J_1BNFLIN WITH HEADER LINE,
        IT_ZNOM_REME_NOTAS     TYPE TABLE OF ZNOM_REME_NOTAS WITH HEADER LINE,
        IT_ZNOM_REME_NOTAS_AUX TYPE TABLE OF ZNOM_REME_NOTAS WITH HEADER LINE,
        VG_LEN                 TYPE I,
        VG_LINES               TYPE I,
        VG_TABIX               TYPE SY-TABIX.

  DATA :BEGIN OF IT_NF OCCURS 0,
          NF TYPE C LENGTH 10,
        END OF IT_NF,

        ULT_NF TYPE C LENGTH 10.

  SELECT * INTO TABLE IT_ZNOM_REME_NOTAS
    FROM ZNOM_REME_NOTAS
   WHERE ID_NOMEACAO_TRAN EQ WA_ZNOM_REMETENTE-ID_NOMEACAO_TRAN
     AND ID_EMPRESA       EQ WA_ZNOM_REMETENTE-ID_EMPRESA
     AND ID_FILIAL        EQ WA_ZNOM_REMETENTE-ID_FILIAL
     AND ID_MATERIAL      EQ WA_ZNOM_REMETENTE-ID_MATERIAL
     AND ID_REMETENTE     EQ WA_ZNOM_REMETENTE-ID_REMETENTE
     AND GRP_RETORNO      EQ WA_ZNOM_REMETENTE-GRP_RETORNO.

  IF NOT IT_ZNOM_REME_NOTAS[] IS INITIAL.

    CLEAR: IT_ZNOM_REME_NOTAS_AUX[].
    MOVE IT_ZNOM_REME_NOTAS[] TO IT_ZNOM_REME_NOTAS_AUX[].
    SORT IT_ZNOM_REME_NOTAS_AUX BY DOCNUM.
    DELETE ADJACENT DUPLICATES FROM IT_ZNOM_REME_NOTAS_AUX COMPARING DOCNUM.

    SELECT * INTO TABLE IT_J_1BNFDOC
      FROM J_1BNFDOC
       FOR ALL ENTRIES IN IT_ZNOM_REME_NOTAS_AUX
     WHERE DOCNUM EQ IT_ZNOM_REME_NOTAS_AUX-DOCNUM.

    CLEAR: IT_ZNOM_REME_NOTAS_AUX[].
    MOVE IT_ZNOM_REME_NOTAS[] TO IT_ZNOM_REME_NOTAS_AUX[].
    SORT IT_ZNOM_REME_NOTAS_AUX BY DOCNUM ITMNUM.
    DELETE ADJACENT DUPLICATES FROM IT_ZNOM_REME_NOTAS_AUX COMPARING DOCNUM ITMNUM.

    SELECT * INTO TABLE IT_J_1BNFLIN
      FROM J_1BNFLIN
       FOR ALL ENTRIES IN IT_ZNOM_REME_NOTAS_AUX
     WHERE DOCNUM EQ IT_ZNOM_REME_NOTAS_AUX-DOCNUM
       AND ITMNUM EQ IT_ZNOM_REME_NOTAS_AUX-ITMNUM.

    VG_HTML = '  <tr><td colspan="2"></td><td colspan="4">'.
*    perform add tables it_html using vg_html.
*    vg_html = '<td width="35%" colspan="4" align="center" bgcolor="FFFFFF">'.
    PERFORM ADD TABLES IT_HTML USING VG_HTML.

    "Tabela de notas fiscais de entrada do produtor ********************************************
    PERFORM ADD TABLES IT_HTML USING '<table cellspacing="0" border="1" bordercolor="FFFFFF" width="100%">'.

    "Modelo
    "Número
    "Série
    "Quantidade
    CONCATENATE '<tr>'
                '<td bgcolor="B0C4DE" width="35%"><p align="center"><font color="#000030" size=1><b>Notas</b></font></p></td>'
                '<td></td>'
                '<td></td>'
*                '<td width="15%"><p align="center"><font color="#000030" size=1><b>Modelo</b></font></p></td>'
*                '<td width="20%"><p align="center"><font color="#000030" size=1><b>Número</b></font></p></td>'
*                '<td width="20%"><p align="center"><font color="#000030" size=1><b>Série</b></font></p></td>'
*                '<td width="45%"><p align="center"><font color="#000030" size=1><b>Quantidade(kg)</b></font></p></td>'
                '</tr>' INTO VG_HTML.
    PERFORM ADD TABLES IT_HTML USING VG_HTML.
    CLEAR: VG_NUMNFE, VG_SEPARATE.
    LOOP AT IT_ZNOM_REME_NOTAS INTO WA_ZNOM_REME_NOTAS.
      READ TABLE IT_J_1BNFDOC WITH KEY DOCNUM = WA_ZNOM_REME_NOTAS-DOCNUM.
      READ TABLE IT_J_1BNFLIN WITH KEY DOCNUM = WA_ZNOM_REME_NOTAS-DOCNUM
                                       ITMNUM = WA_ZNOM_REME_NOTAS-ITMNUM.
      IF IT_J_1BNFDOC-NFE IS INITIAL.
        SHIFT:  IT_J_1BNFDOC-NFNUM LEFT DELETING LEADING '0'.
*        concatenate vg_numnfe it_j_1bnfdoc-nfnum into vg_numnfe SEPARATED BY vg_separate.
        IT_NF-NF = IT_J_1BNFDOC-NFNUM.
        APPEND IT_NF.
      ELSE.
        SHIFT: IT_J_1BNFDOC-NFENUM LEFT DELETING LEADING '0'.
*        concatenate vg_numnfe it_j_1bnfdoc-nfenum into vg_numnfe SEPARATED BY vg_separate.
        IT_NF-NF = IT_J_1BNFDOC-NFENUM.
        APPEND IT_NF.
      ENDIF.
    ENDLOOP.

    SORT IT_NF BY NF.

    VG_LINES = LINES( IT_NF ).

    LOOP AT IT_NF.
      VG_TABIX = SY-TABIX.

      IF VG_NUMNFE IS NOT INITIAL.
        ADD 1 TO ULT_NF.
        CONDENSE ULT_NF NO-GAPS.
        IF ULT_NF = IT_NF-NF.
          VG_SEPARATE = 'a'.
          IF ( VG_TABIX EQ VG_LINES ).
            CONCATENATE VG_NUMNFE IT_NF-NF INTO VG_NUMNFE SEPARATED BY VG_SEPARATE.
          ELSE.
            CONTINUE.
          ENDIF.
        ELSE.
          IF VG_SEPARATE = 'a'.
            SUBTRACT 1 FROM ULT_NF.
            CONDENSE ULT_NF NO-GAPS.
            CONCATENATE VG_NUMNFE 'a' ULT_NF INTO VG_NUMNFE SEPARATED BY SPACE.
          ENDIF.
          VG_SEPARATE = ','.
        ENDIF.
      ELSE.
        VG_SEPARATE = ''.
      ENDIF.
      CONCATENATE VG_NUMNFE IT_NF-NF INTO VG_NUMNFE SEPARATED BY VG_SEPARATE.
      ULT_NF = IT_NF-NF.
    ENDLOOP.

    CONCATENATE '<tr bordercolor="black"><td colspan="4"><p align="left"> <font color="000" size=1><b>' VG_NUMNFE '</b></font></p></td></tr>' INTO VG_HTML.
    PERFORM ADD TABLES IT_HTML USING VG_HTML.

    VG_LEN = STRLEN( VG_NUMNFE ) - 1.

    MOVE VG_LEN TO VG_NUMCHAR.

    CONCATENATE '<tr bordercolor="black"><td colspan="4"><p align="left"> <font color="000" size=1>' VG_NUMCHAR 'Caracteres' '</font></p></td></tr>' INTO VG_HTML SEPARATED BY SPACE.
    PERFORM ADD TABLES IT_HTML USING VG_HTML.
    PERFORM ADD TABLES IT_HTML USING '</table>'.
    "*******************************************************************************************
    "*******************************************************************************************
    VG_HTML = '</td></tr>'.
    PERFORM ADD TABLES IT_HTML USING VG_HTML.

  ENDIF.

ENDFUNCTION.
