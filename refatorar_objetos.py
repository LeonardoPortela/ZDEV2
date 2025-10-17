#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script para refatoração de objetos ABAP
Mantém apenas objetos de dicionário de dados utilizados pelos objetos principais
"""

import os
import re
import glob
from pathlib import Path
from typing import Set, Dict, List

# Lista de objetos principais que devem ser mantidos
OBJETOS_PRINCIPAIS = [
    'ZCL_DOC_ELETRONICO',
    'ZIF_DOC_ELETRONICO',
    'ZCL_WEBSERVICE',
    'ZIF_WEBSERVICE',
    'ZBRNFE_DANFE',
    'ZFSD_BUSCA_DANFE',
    'ZDEQUEUE_ALL',
    'ZGRC_LIMPA_REF_MIRO_FISCAL',
    'Z_DETALHAMENTO_CTE',
    'Z_DETALHAMENTO_CTE_IN_MASSA',
    'Z_DETALHAMENTO_CTE_XML',
    'Z_DETALHAMENTO_NFE',
    'Z_GRC_AJUSTA_TP_EMISSAO',
    'Z_GRC_ARQUIVO_DOC',
    'Z_GRC_ARQUIVO_DOC_XML',
    'Z_GRC_DOWNLOAD_XML_PDF',
    'Z_GRC_ENVIA_LEGADO',
    'Z_GRC_GET_STATUS_DOC',
    'Z_GRC_MDFE_AVULSA',
    'Z_GRC_MDFE_LOAD',
    'Z_GRC_MONTA_LINK',
    'Z_GRC_NEW_NFE',
    'Z_GRC_REGISTRA_INF_ZIB_NFE',
    'Z_GRC_REGISTRA_LOG_DOC',
    'Z_GRC_SEND_EMAIL_AUTO',
    'Z_J_1BMFE_CANCEL_EVENT_SEND',
    'Z_J_1B_EVENT_CANCEL_NF_CTE',
    'Z_J_1B_MDFE_CANCEL',
    'Z_J_1B_MDFE_CLOSE',
    'Z_J_1B_MDFE_XML_OUT',
    'Z_J_1B_NF_OBJECT_ADD',
    'Z_SHOW_DETALHAMENTO_CTE',
    'Z_SHOW_DETALHAMENTO_NFE',
]

# Padrões para identificar referências a objetos de dicionário de dados
PATTERNS = {
    'tables': [
        r'FROM\s+([Z][A-Z0-9_]+)',
        r'INTO\s+TABLE\s+([Z][A-Z0-9_]+)',
        r'TABLE\s+([Z][A-Z0-9_]+)',
        r'TABLES?\s*:\s*([Z][A-Z0-9_]+)',
    ],
    'structures': [
        r'TYPE\s+([Z][A-Z0-9_]+)',
        r'LIKE\s+([Z][A-Z0-9_]+)',
        r'STRUCTURE\s+([Z][A-Z0-9_]+)',
    ],
    'data_elements': [
        r'TYPE\s+([Z][A-Z0-9_]+)',
    ],
    'domains': [
        r'DOMAIN\s+([Z][A-Z0-9_]+)',
    ],
    'function_modules': [
        r'FUNCTION\s+[\'"]([Z][A-Z0-9_]+)[\'"]',
        r'CALL\s+FUNCTION\s+[\'"]([Z][A-Z0-9_]+)[\'"]',
    ],
}

class RefatoradorObjetos:
    def __init__(self, src_path: str):
        self.src_path = Path(src_path)
        self.objetos_manter: Set[str] = set(OBJETOS_PRINCIPAIS)
        self.objetos_dicionario: Set[str] = set()
        self.objetos_programa: Set[str] = set()
        self.dependencias: Dict[str, Set[str]] = {}
        
    def mapear_arquivos_principais(self) -> Dict[str, List[Path]]:
        """Mapeia todos os arquivos dos objetos principais"""
        arquivos = {}
        
        for obj in OBJETOS_PRINCIPAIS:
            obj_lower = obj.lower()
            # Procurar arquivos relacionados
            patterns = [
                f"**/{obj_lower}.*.abap",
                f"**/{obj_lower}.*.xml",
            ]
            
            files = []
            for pattern in patterns:
                files.extend(self.src_path.glob(pattern))
            
            if files:
                arquivos[obj] = files
                print(f"✓ Encontrado objeto: {obj} ({len(files)} arquivos)")
        
        return arquivos
    
    def extrair_referencias(self, arquivo: Path) -> Set[str]:
        """Extrai todas as referências a objetos de um arquivo ABAP"""
        referencias = set()
        
        try:
            with open(arquivo, 'r', encoding='utf-8', errors='ignore') as f:
                conteudo = f.read().upper()
                
                # Buscar por todos os padrões
                for tipo, patterns in PATTERNS.items():
                    for pattern in patterns:
                        matches = re.findall(pattern, conteudo, re.IGNORECASE | re.MULTILINE)
                        for match in matches:
                            if match.startswith('Z'):
                                referencias.add(match.upper())
        
        except Exception as e:
            print(f"⚠ Erro ao ler {arquivo}: {e}")
        
        return referencias
    
    def analisar_dependencias(self, arquivos_principais: Dict[str, List[Path]]):
        """Analisa todas as dependências recursivamente"""
        print("\n" + "="*80)
        print("ANALISANDO DEPENDÊNCIAS")
        print("="*80)
        
        # Fila de objetos para processar
        fila = list(OBJETOS_PRINCIPAIS)
        processados = set()
        
        while fila:
            obj_atual = fila.pop(0)
            
            if obj_atual in processados:
                continue
            
            processados.add(obj_atual)
            print(f"\nProcessando: {obj_atual}")
            
            # Procurar arquivos do objeto atual
            obj_lower = obj_atual.lower()
            arquivos = list(self.src_path.glob(f"**/{obj_lower}.*.abap"))
            
            if not arquivos:
                continue
            
            # Extrair referências de todos os arquivos do objeto
            referencias = set()
            for arquivo in arquivos:
                refs = self.extrair_referencias(arquivo)
                referencias.update(refs)
            
            self.dependencias[obj_atual] = referencias
            
            # Adicionar novas referências à fila
            for ref in referencias:
                if ref not in processados and ref not in fila:
                    fila.append(ref)
                    print(f"  → Nova dependência: {ref}")
    
    def identificar_objetos_dicionario(self):
        """Identifica todos os objetos de dicionário de dados que devem ser mantidos"""
        print("\n" + "="*80)
        print("IDENTIFICANDO OBJETOS DE DICIONÁRIO")
        print("="*80)
        
        # Extensões de objetos de dicionário de dados
        extensoes_dicionario = [
            '.tabl.xml',    # Tabelas
            '.dtel.xml',    # Elementos de dados
            '.doma.xml',    # Domínios
            '.ttyp.xml',    # Tipos de tabela
            '.shlp.xml',    # Search helps
            '.msag.xml',    # Classes de mensagem
        ]
        
        # Todas as referências encontradas
        todas_referencias = set()
        for refs in self.dependencias.values():
            todas_referencias.update(refs)
        
        # Procurar arquivos de dicionário para cada referência
        for ref in todas_referencias:
            ref_lower = ref.lower()
            
            for ext in extensoes_dicionario:
                pattern = f"**/{ref_lower}{ext}"
                arquivos = list(self.src_path.glob(pattern))
                
                if arquivos:
                    self.objetos_dicionario.add(ref)
                    self.objetos_manter.add(ref)
                    print(f"✓ Objeto de dicionário mantido: {ref} ({ext})")
    
    def listar_objetos_para_excluir(self) -> List[Path]:
        """Lista todos os objetos que devem ser excluídos"""
        print("\n" + "="*80)
        print("IDENTIFICANDO OBJETOS PARA EXCLUSÃO")
        print("="*80)
        
        objetos_excluir = []
        
        # Listar todos os arquivos na pasta src
        todos_arquivos = list(self.src_path.rglob("*"))
        
        for arquivo in todos_arquivos:
            if not arquivo.is_file():
                continue
            
            # Extrair nome do objeto do arquivo
            nome_arquivo = arquivo.stem.split('.')[0].upper()
            
            # Verificar se deve ser mantido
            deve_manter = False
            for obj_manter in self.objetos_manter:
                if nome_arquivo == obj_manter or nome_arquivo.startswith(obj_manter + '.'):
                    deve_manter = True
                    break
            
            if not deve_manter:
                # Verificar se é um objeto que não é programa/classe/interface
                extensoes_excluir = ['.prog.', '.clas.', '.intf.', '.fugr.']
                eh_programa = any(ext in str(arquivo) for ext in extensoes_excluir)
                
                if eh_programa or nome_arquivo.startswith('Z'):
                    objetos_excluir.append(arquivo)
        
        return objetos_excluir
    
    def excluir_objetos(self, objetos: List[Path]):
        """Exclui os objetos não utilizados"""
        print("\n" + "="*80)
        print(f"EXCLUINDO {len(objetos)} OBJETOS NÃO UTILIZADOS")
        print("="*80)
        
        for arquivo in objetos:
            try:
                arquivo.unlink()
                print(f"✓ Excluído: {arquivo.relative_to(self.src_path)}")
            except Exception as e:
                print(f"⚠ Erro ao excluir {arquivo}: {e}")
    
    def gerar_relatorio(self):
        """Gera relatório da refatoração"""
        print("\n" + "="*80)
        print("RELATÓRIO FINAL")
        print("="*80)
        
        print(f"\nObjetos principais mantidos: {len(OBJETOS_PRINCIPAIS)}")
        print(f"Objetos de dicionário mantidos: {len(self.objetos_dicionario)}")
        print(f"Total de objetos mantidos: {len(self.objetos_manter)}")
        
        print("\n\nOBJETOS DE DICIONÁRIO MANTIDOS:")
        for obj in sorted(self.objetos_dicionario):
            print(f"  - {obj}")
    
    def executar(self):
        """Executa o processo completo de refatoração"""
        print("="*80)
        print("REFATORAÇÃO DE OBJETOS ABAP")
        print("="*80)
        print(f"Diretório: {self.src_path}")
        
        # 1. Mapear arquivos principais
        arquivos_principais = self.mapear_arquivos_principais()
        
        # 2. Analisar dependências
        self.analisar_dependencias(arquivos_principais)
        
        # 3. Identificar objetos de dicionário
        self.identificar_objetos_dicionario()
        
        # 4. Listar objetos para excluir
        objetos_excluir = self.listar_objetos_para_excluir()
        
        # 5. Gerar relatório
        self.gerar_relatorio()
        
        # 6. Confirmar exclusão
        print("\n" + "="*80)
        print(f"ATENÇÃO: {len(objetos_excluir)} objetos serão excluídos!")
        print("="*80)
        
        resposta = input("\nDeseja prosseguir com a exclusão? (sim/não): ")
        
        if resposta.lower() == 'sim':
            self.excluir_objetos(objetos_excluir)
            print("\n✓ Refatoração concluída com sucesso!")
        else:
            print("\n✓ Refatoração cancelada. Nenhum arquivo foi excluído.")

if __name__ == "__main__":
    src_path = "src"
    refatorador = RefatoradorObjetos(src_path)
    refatorador.executar()


