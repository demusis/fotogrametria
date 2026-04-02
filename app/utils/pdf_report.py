"""
Geração de Relatório PDF utilizando QTextDocument (PySide6).
"""

import io
import base64
import datetime
import numpy as np
from matplotlib.figure import Figure

from PySide6.QtGui import QTextDocument, QPageSize, QPageLayout
from PySide6.QtPrintSupport import QPrinter
from PySide6.QtCore import QMarginsF

def figura_para_base64(fig: Figure) -> str:
    """Converte figura Matplotlib para Base64 PNG."""
    buf = io.BytesIO()
    # Garante fundo branco para o PDF
    bg_color = fig.get_facecolor()
    fig.patch.set_facecolor('white')
    
    # Renderiza e converte
    fig.savefig(buf, format='png', dpi=150, bbox_inches='tight', facecolor='white', edgecolor='white')
    
    # Restaura o fundo original
    fig.patch.set_facecolor(bg_color)
    
    buf.seek(0)
    return base64.b64encode(buf.read()).decode('utf-8')

def gerar_pdf_academico(
    caminho_saida: str,
    dados_pontos: list[dict] | None,
    parametros: dict | None,
    resultado,  # ResultadoProcessamento | None
    figuras: list[tuple[str, Figure]],
    fig_regressao: Figure | None,
    resultados_mardia: list[dict] | None,
    audit_log: list[str] | None
):
    """
    Gera e salva um relatório em PDF estruturado academicamente
    utilizando QPrinter do PySide6.
    """
    
    html = []
    
    # CSS focado em impressão e padrão acadêmico
    html.append("""
    <html><head><style>
        body { 
            font-family: "Times New Roman", Times, serif; 
            font-size: 11pt; 
            line-height: 1.5; 
            text-align: justify; 
            color: black; 
        }
        h1 { 
            font-size: 16pt; 
            text-align: center; 
            text-transform: uppercase; 
            margin-bottom: 24px; 
            font-weight: bold; 
        }
        h2 { 
            font-size: 14pt; 
            border-bottom: 1px solid #000; 
            padding-bottom: 4px; 
            margin-top: 24px; 
            font-weight: bold; 
        }
        h3 { 
            font-size: 12pt; 
            margin-top: 16px; 
            font-weight: bold; 
        }
        table { 
            border-collapse: collapse; 
            margin: 16px auto; 
            width: 80%; 
        }
        th, td { 
            border: 1px solid #000; 
            padding: 6px 12px; 
            text-align: center; 
        }
        th { 
            background-color: #f2f2f2; 
            font-weight: bold; 
        }
        .center { 
            text-align: center; 
        }
        .caption { 
            text-align: center; 
            font-size: 10pt; 
            margin-top: 16px; 
            margin-bottom: 4px;
        }
        .img-container { 
            text-align: center; 
            margin-top: 16px; 
        }
        img { 
            width: 600px;
        }
        .log-box {
            font-family: "Courier New", Courier, monospace; 
            font-size: 9pt; 
            background-color: #f8f9fa; 
            padding: 10px; 
            border: 1px solid #cccccc;
            text-align: left;
        }
    </style></head><body>
    """)
    
    # Capa / Título
    html.append("<h1>Relatório Pericial<br>Estimativa de Velocidade Veicular</h1>")
    data_str = datetime.datetime.now().strftime("%d/%m/%Y às %H:%M:%S")
    html.append(f"<p class='center'>Data de Geração: {data_str}</p>")
    
    # 1. Introdução
    html.append("<h2>1. Introdução</h2>")
    html.append("<p>Este relatório apresenta os resultados analíticos da estimativa de velocidade veicular baseada no método fotogramétrico da razão cruzada. O procedimento fundamenta-se metodologicamente no artigo científico publicado na Revista Brasileira de Criminalística (disponível em <a href='https://revista.rbc.org.br/index.php/rbc/article/view/111'>https://revista.rbc.org.br/index.php/rbc/article/view/111</a>), utilizando marcações de coordenadas e iterando através de simulações de Monte Carlo acopladas à regressão ortogonal de Deming. Isso permite inferir analiticamente a propagação de incertezas temporais relativas à taxa de quadros associadas.</p>")
    html.append("<p>O código-fonte do aplicativo utilizado neste laudo é de domínio aberto e encontra-se disponível no repositório: <a href='https://github.com/demusis/fotogrametria'>https://github.com/demusis/fotogrametria</a>.</p>")
    
    # 2. Configurações e Parâmetros
    html.append("<h2>2. Parâmetros e Simulação</h2>")
    if parametros:
        html.append("<table style='page-break-inside: avoid; width: 100%; border: none; margin: 0; padding: 0;'><tr><td style='border: none; padding: 0;'>")
        html.append("<div class='caption'>Tabela 1: Parâmetros delimitadores utilizados durante o processamento.</div>")
        html.append("<table><tr><th>Parâmetro</th><th>Valor Utilizado</th></tr>")
        param_nomes = {
            "inicio_quadro": "Tempo do Quadro Inicial (s)",
            "fim_quadro": "Tempo do Quadro Final (s)",
            "erro_medio_mt": "Erro Médio da Marcação Temporal (ms/s)",
            "dp_erro_medio_mt": "Desvio Padrão do Erro (ms/s)",
            "dist_referencia": "Distância de Referência (mm)",
            "sigma_filtro": "Sigma do Filtro (Pré-proc)",
            "repeticoes": "Nº Repetições (Monte Carlo)",
            "nivel_confianca": "Nível de Confiança"
        }
        for key, name in param_nomes.items():
            val = parametros.get(key, "")
            html.append(f"<tr><td>{name}</td><td>{val}</td></tr>")
        html.append("</table>")
        html.append("</td></tr></table>")
    else:
        html.append("<p>Parâmetros não fornecidos.</p>")
    
    # 3. Modelagem e Validação
    html.append("<h2>3. Modelagem e Ajuste de Distribuição</h2>")
    
    if fig_regressao:
        b64_reg = figura_para_base64(fig_regressao)
        html.append("<table style='page-break-inside: avoid; width: 100%; border: none; margin: 0; padding: 0;'><tr><td style='border: none; padding: 0;'>")
        html.append("<div class='caption'>Figura 1: Representação gráfica do modelo linear de Deming sobre a imagem evidência.</div>")
        html.append(f"<div class='img-container'><img src='data:image/png;base64,{b64_reg}' width='500'></div>")
        html.append("</td></tr></table>")
    
    if resultados_mardia:
        html.append("<h3>3.1. Teste de Normalidade Multivariado (Mardia)</h3>")
        html.append("<table style='page-break-inside: avoid; width: 100%; border: none; margin: 0; padding: 0;'><tr><td style='border: none; padding: 0;'>")
        html.append("<div class='caption'>Tabela 2: Resultados da normalidade multivariada dos grupamentos em estudo.</div>")
        html.append("<table><tr><th>Grupo Amostral</th><th>Classificação</th><th>P-Valor (Assimetria)</th><th>P-Valor (Curtose)</th></tr>")
        for rm in resultados_mardia:
            html.append(f"<tr><td>{rm['grupo']}</td><td>{rm['resultado']}</td><td>{rm['skewness_p']}</td><td>{rm['kurtosis_p']}</td></tr>")
        html.append("</table>")
        html.append("</td></tr></table>")
        
    html.append("<div style='page-break-before: always;'></div>")
        
    # 4. Resultados Estatísticos
    html.append("<h2>4. Resultados Finais e Estatística</h2>")
    if resultado:
        html.append("<h3>4.1. Estimativas Computadas</h3>")
        html.append("<table style='page-break-inside: avoid; width: 100%; border: none; margin: 0; padding: 0;'><tr><td style='border: none; padding: 0;'>")
        html.append("<div class='caption'>Tabela 3: Síntese dos quantis apurados na convergência de Monte Carlo.</div>")
        html.append("<table><tr><th>Métrica (Grandeza)</th><th>Estimativa Central (Média)</th><th>Intervalo de Confiança</th></tr>")
        
        ic_vel = f"[{resultado.vel_percentis[0]:.2f} - {resultado.vel_percentis[1]:.2f}]"
        html.append(f"<tr><td>Velocidade (km/h)</td><td><b>{resultado.vel_media:.2f}</b></td><td>{ic_vel}</td></tr>")
        
        ic_desl = f"[{resultado.desl_percentis[0]:.3f} - {resultado.desl_percentis[1]:.3f}]"
        html.append(f"<tr><td>Deslocamento (m)</td><td>{resultado.desl_media:.3f}</td><td>{ic_desl}</td></tr>")
        html.append("</table>")
        html.append("</td></tr></table>")
        
        if figuras:
            html.append("<h3>4.2. Análise Gráfica</h3>")
            figura_idx = 2
            for nome_fig, fig in figuras:
                b64_fig = figura_para_base64(fig)
                
                # Cleanup title from filename
                titulo_bonito = nome_fig.replace("1_", "").replace("2_", "").replace("3_", "").replace("4_", "").replace(".png", "").replace("_", " ").title()
                html.append("<table style='page-break-inside: avoid; width: 100%; border: none; margin: 0; padding: 0;'><tr><td style='border: none; padding: 0;'>")
                html.append(f"<div class='caption'>Figura {figura_idx}: Representação referente a(o) {titulo_bonito}.</div>")
                html.append(f"<div class='img-container'><img src='data:image/png;base64,{b64_fig}' width='500'></div>")
                html.append("</td></tr></table>")
                figura_idx += 1
    else:
        html.append("<p>O processamento computacional não conferiu resultados passíveis de exibição.</p>")
        
    html.append("<div style='page-break-before: always;'></div>")
        
    # 5. Dados Brutos
    html.append("<h2>5. Dados Coletados na Referência</h2>")
    if dados_pontos:
        html.append("<table style='page-break-inside: avoid; width: 100%; border: none; margin: 0; padding: 0;'><tr><td style='border: none; padding: 0;'>")
        html.append("<div class='caption'>Tabela 4: Amostragem empírica de coordenadas.</div>")
        html.append("<table><tr><th>Grupamento</th><th>Coordenada X (px)</th><th>Coordenada Y (px)</th><th>Intensidade (Cinza)</th></tr>")
        for p in dados_pontos:
            html.append(f"<tr><td>{p['ponto']}</td><td>{p['x']}</td><td>{p['y']}</td><td>{p['cinza']:.4f}</td></tr>")
        html.append("</table>")
        html.append("</td></tr></table>")
    else:
        html.append("<p>Ausência de dados de entrada anotados.</p>")
        
    # 6. Audit Trail
    if audit_log:
        html.append("<h2>6. Trilha de Auditoria</h2>")
        html.append("<p>Abaixo encontra-se o registro cronológico das ações computacionais e interativas efetuadas no sistema:</p>")
        html.append("<div class='log-box'>")
        for log in audit_log:
            html.append(f"{log}<br>")
        html.append("</div>")
        
    html.append("</body></html>")
    
    html_text = "".join(html)
    
    # Geração final
    doc = QTextDocument()
    doc.setHtml(html_text)
    
    printer = QPrinter(QPrinter.PrinterMode.HighResolution)
    printer.setOutputFormat(QPrinter.OutputFormat.PdfFormat)
    printer.setOutputFileName(caminho_saida)
    
    # Configurações para A4 com margens padrão
    printer.setPageSize(QPageSize(QPageSize.PageSizeId.A4))
    printer.setPageMargins(QMarginsF(15, 15, 15, 15), QPageLayout.Unit.Millimeter)
    
    doc.print_(printer)
