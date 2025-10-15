"Name: \PR:SAPLMMTE\FO:PBO_TREE\SE:END\EI
ENHANCEMENT 0 ZMM_ENH_FORMAT_TEXTO.

 "MM - ZMM0149 - ZMM0149 - Adicionar campos  #138115 RSA
 "Formata Texto do Pedido Criado pela Transação ZMM0149
 IF vg_editor_type IS INITIAL.
   READ TABLE gt_control WITH KEY object  = g_textobject.
   IF sy-subrc EQ 0.
     l_tabix = sy-tabix.
     gt_control-editor_type = 2.
     mepotext-editor = gt_control-editor_type.
     MODIFY gt_control INDEX l_tabix.
     vg_editor_type = abap_true.
   ENDIF.
 ENDIF.
 "MM - ZMM0149 - ZMM0149 - Adicionar campos  #138115 RSA

ENDENHANCEMENT.
