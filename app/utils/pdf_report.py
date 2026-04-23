"""
Geração de Relatório PDF utilizando LaTeX (pdflatex).
"""

import os
import subprocess
import tempfile
import shutil
import datetime
from pathlib import Path
from matplotlib.figure import Figure

def gerar_pdf_academico(
    caminho_saida: str,
    dados_pontos: list[dict] | None,
    parametros: dict | None,
    resultado,  # ResultadoProcessamento | None
    figuras: list[tuple[str, Figure]],
    fig_regressao: Figure | None,
    resultados_mardia: list[dict] | None,
    audit_log: list[str] | None,
    video_info: dict | None = None,
    regression_metrics: dict | None = None
):
    """
    Gera e salva um relatório em PDF estruturado academicamente
    utilizando LaTeX (pdflatex).
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        tmp_path = Path(tmpdir)
        tex_path = tmp_path / "report.tex"
        
        # Helper for sanitizing text for LaTeX
        def sanitize(text: str) -> str:
            if not text: return ""
            chars = {
                '&': r'\&', '%': r'\%', '$': r'\$', '#': r'\#',
                '_': r'\_', '{': r'\{', '}': r'\}', '~': r'\textasciitilde{}',
                '^': r'\textasciicircum{}', '\\': r'\textbackslash{}'
            }
            res = ""
            for char in str(text):
                res += chars.get(char, char)
            return res
            
        tex = []
        tex.append(r"\documentclass[12pt,a4paper]{article}")
        tex.append(r"\usepackage[utf8]{inputenc}")
        tex.append(r"\usepackage[T1]{fontenc}")
        tex.append(r"\usepackage[brazil]{babel}")
        tex.append(r"\usepackage{graphicx}")
        tex.append(r"\usepackage{geometry}")
        tex.append(r"\geometry{a4paper, margin=2.5cm}")
        tex.append(r"\usepackage{booktabs}")
        tex.append(r"\usepackage{float}")
        tex.append(r"\usepackage[hidelinks]{hyperref}")
        tex.append(r"\usepackage{longtable}")
        tex.append(r"\usepackage{caption}")
        tex.append(r"\captionsetup{justification=centering, font=small}")
        
        tex.append(r"\begin{document}")
        tex.append(r"\title{\vspace{-2cm}Relatório Pericial \\ \large Estimativa de Velocidade Veicular}")
        data_str = datetime.datetime.now().strftime("%d/%m/%Y às %H:%M:%S")
        tex.append(rf"\date{{Data de Geração: {data_str}}}")
        tex.append(r"\maketitle")
        
        tex.append(r"\section{Introdução}")
        
        import textwrap
        def wrap_verbatim(text, width=70):
            lines = []
            for line in text.split('\n'):
                wrapped = textwrap.wrap(line, width=width)
                if not wrapped:
                    lines.append("")
                else:
                    lines.extend(wrapped)
            return lines

        if video_info:
            tex.append(r"\subsection{Dados do Arquivo de Vídeo Original}")
            
            def make_breakable(t, step=8):
                sanitized = sanitize(t)
                return r"\allowbreak{}".join(sanitized[i:i+step] for i in range(0, len(sanitized), step))
                
            tex.append(rf"\textbf{{Arquivo:}} \texttt{{{make_breakable(video_info['filename'], 15)}}} \\")
            tex.append(rf"\textbf{{Hash SHA-512:}} \texttt{{{make_breakable(video_info['sha512'], 8)}}} \\")
            tex.append(r"\textbf{Metadados (MediaInfo / FFprobe):}")
            tex.append(r"\begin{verbatim}")
            for wl in wrap_verbatim(video_info['mediainfo'].strip()):
                tex.append(wl)
            tex.append(r"\end{verbatim}")

        tex.append(r"\subsection{Metodologia}")
        tex.append(r"O modelo algorítmico e a estratégia de mensuração fotogramétrica adotados neste processamento fundamentam-se na metodologia de Razão Cruzada Complexa associada à simulação estocástica, conforme desenvolvido e validado por De Musis et al. (2025) e publicado na \textit{Revista Brasileira de Criminalística} (disponível em \url{https://revista.rbc.org.br/index.php/rbc/article/view/936}). Adicionalmente, o código-fonte do software utilizado na geração deste laudo é de domínio público (\textit{open source}) e encontra-se permanentemente aberto para auditoria de terceiros no repositório \url{https://github.com/demusis/fotogrametria}.")
        tex.append(r"")
        tex.append(r"A incerteza metrológica posicional e temporal foi estimada por meio do \textbf{Método de Monte Carlo}, um procedimento probabilístico recomendado pelo \textit{Bureau International des Poids et Mesures} (BIPM) no GUM (\textit{Guide to the Expression of Uncertainty in Measurement}) para lidar com propagações complexas de erros e não linearidades. O método consiste em realizar milhares de amostragens aleatórias a partir de uma distribuição de probabilidade atribuída às variáveis de entrada para simular numericamente a distribuição empírica da variável de saída (BIPM, 2008).")
        tex.append(r"")
        tex.append(r"A trajetória do deslocamento foi otimizada utilizando-se a \textbf{Regressão de Deming} (também conhecida como Regressão Ortogonal ou de Mínimos Quadrados Totais). Diferentemente da regressão linear simples que minimiza os resíduos apenas no eixo $Y$, a Regressão de Deming minimiza a distância ortogonal entre os pontos empíricos e a reta de predição, levando em consideração de forma equitativa os erros de observação intrínsecos a ambos os eixos cartesianos ($X$ e $Y$), fornecendo um estimador ajustado em cenários de medição fotogramétrica bidimensional (DEMING, 1943; LINNET, 1993).")
        tex.append(r"")
        tex.append(r"O nível de confiança adotado para o cálculo dos intervalos de incerteza foi de \textbf{" + f"{parametros.get('nivel_confianca', 99)}" + r"\%}.")
        
        tex.append(r"\section{Parâmetros e Simulação}")
        if parametros:
            tex.append(r"\begin{table}[H]")
            tex.append(r"\centering")
            tex.append(r"\caption{Parâmetros delimitadores utilizados durante o processamento.}")
            tex.append(r"\begin{tabular}{ll}")
            tex.append(r"\toprule")
            tex.append(r"\textbf{Parâmetro} & \textbf{Valor Utilizado} \\")
            tex.append(r"\midrule")
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
                tex.append(rf"{sanitize(name)} & {sanitize(str(val))} \\")
            tex.append(r"\bottomrule")
            tex.append(r"\end{tabular}")
            tex.append(r"\end{table}")
        else:
            tex.append(r"Parâmetros não fornecidos.")
            
        tex.append(r"\section{Modelagem e Ajuste de Distribuição}")
        
        if regression_metrics:
            tex.append(r"\subsection{Regressão Ortogonal (Deming)}")
            tex.append(r"\begin{itemize}")
            tex.append(rf"\item \textbf{{Equação:}} {sanitize(regression_metrics.get('equacao', ''))}")
            tex.append(rf"\item \textbf{{Coeficiente de Determinação (R\textsuperscript{{2}}):}} {sanitize(str(regression_metrics.get('r2', '')))}")
            tex.append(rf"\item \textbf{{Critério de Informação de Akaike (AIC):}} {sanitize(str(regression_metrics.get('aic', '')))}")
            tex.append(r"\end{itemize}")

        # Salvar figuras e processar estilos dark mode
        def prepare_and_save_figure(fig, filename):
            # Força o tema claro para exportação (fundo branco, texto preto)
            bg_color = fig.get_facecolor()
            fig.patch.set_facecolor('white')
            axes_styles = []
            for ax in fig.axes:
                axes_styles.append({
                    'facecolor': ax.get_facecolor(),
                    'title_color': ax.title.get_color(),
                    'xlabel_color': ax.xaxis.label.get_color(),
                    'ylabel_color': ax.yaxis.label.get_color(),
                })
                ax.set_facecolor('white')
                ax.tick_params(colors='black')
                ax.xaxis.label.set_color('black')
                ax.yaxis.label.set_color('black')
                ax.title.set_color('black')
                for spine in ax.spines.values():
                    spine.set_color('black')
                    
            fig.savefig(tmp_path / filename, format='png', dpi=200, bbox_inches='tight', facecolor='white', edgecolor='white')
            
            # Restaura
            fig.patch.set_facecolor(bg_color)
            for ax, style in zip(fig.axes, axes_styles):
                ax.set_facecolor(style['facecolor'])
                ax.tick_params(colors=style['xlabel_color'])
                ax.xaxis.label.set_color(style['xlabel_color'])
                ax.yaxis.label.set_color(style['ylabel_color'])
                ax.title.set_color(style['title_color'])

        if fig_regressao:
            prepare_and_save_figure(fig_regressao, "fig_regressao.png")
            tex.append(r"A trajetória do deslocamento foi modelada utilizando a regressão de Deming, que minimiza as distâncias ortogonais entre os pontos amostrados e a linha de tendência. Este método é preferível em aplicações fotogramétricas pois considera as incertezas de medição inerentes a ambos os eixos cartesianos, proporcionando um estimador compatível com a geometria projetiva da cena observada.")
            tex.append(r"\begin{figure}[H]")
            tex.append(r"\centering")
            tex.append(r"\includegraphics[width=1.0\textwidth,height=0.40\textheight,keepaspectratio]{fig_regressao.png}")
            tex.append(r"\caption{Representação gráfica do modelo linear de Deming sobre a imagem evidência.}")
            tex.append(r"\end{figure}")
            
        if resultados_mardia:
            tex.append(r"\subsection{Análise de Normalidade (Testes de Mardia e Shapiro-Wilk)}")
            tex.append(r"Para certificar a aderência estocástica das repetições, foram conduzidos testes de normalidade. O \textbf{Teste de Mardia} examina a normalidade multivariada avaliando a assimetria e a curtose dos dados em dimensões superiores. Para acomodar amostras pequenas, empregou-se o fator de correção de Mardia para baixa amostragem ($N < 20$) a fim de retificar possíveis falhas de inferência estatística (MARDIA, 1970; 1974). Adicionalmente, considerando a redução de sensibilidade do teste de Mardia em amostras reduzidas, efetuou-se a verificação univariada das coordenadas $X$ e $Y$ por intermédio do teste de \textbf{Shapiro-Wilk}, o qual apresenta poder estatístico adequado para amostras com $N < 50$ (SHAPIRO; WILK, 1965). O nível de significância referencial estipulado foi de $\alpha = 0.05$.")
            tex.append(r"\begin{table}[H]")
            tex.append(r"\centering")
            tex.append(r"\begin{tabular}{cccccc}")
            tex.append(r"\toprule")
            tex.append(r"\textbf{Grupo} & \textbf{N} & \textbf{Assimetria ($p$)} & \textbf{Curtose ($p$)} & \textbf{S-W X ($p$)} & \textbf{S-W Y ($p$)} \\")
            tex.append(r"\midrule")
            LETRAS = ['A', 'B', 'C', 'D']
            for i, rm in enumerate(resultados_mardia):
                letra = LETRAS[i] if i < 4 else str(i + 1)
                n_pontos = rm.get('N', '-')
                skew_p = rm.get('skewness_p', '')
                kurt_p = rm.get('kurtosis_p', '')
                shap_x = rm.get('shapiro_x_p', '')
                shap_y = rm.get('shapiro_y_p', '')
                
                s_p_str = f"{float(skew_p):.4f}" if skew_p != '' else "-"
                k_p_str = f"{float(kurt_p):.4f}" if kurt_p != '' else "-"
                sx_p_str = f"{float(shap_x):.4f}" if shap_x != '' else "-"
                sy_p_str = f"{float(shap_y):.4f}" if shap_y != '' else "-"
                
                tex.append(rf"{letra} & {sanitize(str(n_pontos))} & {s_p_str} & {k_p_str} & {sx_p_str} & {sy_p_str} \\")
            tex.append(r"\bottomrule")
            tex.append(r"\end{tabular}")
            tex.append(r"\caption{Resultados estatísticos das margens de distribuição multivariada e univariada por grupo de coleta.}")
            tex.append(r"\end{table}")
            
        tex.append(r"\clearpage")
        tex.append(r"\section{Resultados Finais e Estatística}")
        if resultado:
            tex.append(r"\subsection{Estimativas Computadas}")
            tex.append(r"\begin{table}[H]")
            tex.append(r"\centering")
            tex.append(r"\caption{Síntese dos quantis apurados na convergência de Monte Carlo.}")
            tex.append(r"\begin{tabular}{lcc}")
            tex.append(r"\toprule")
            tex.append(r"\textbf{Métrica} & \textbf{Estimativa Central} & \textbf{Intervalo de Confiança} \\")
            tex.append(r"\midrule")
            ic_vel = f"[{resultado.vel_percentis[0]:.2f} - {resultado.vel_percentis[1]:.2f}]"
            tex.append(rf"Velocidade (km/h) & \textbf{{{resultado.vel_media:.2f}}} & {ic_vel} \\")
            ic_desl = f"[{resultado.desl_percentis[0]:.3f} - {resultado.desl_percentis[1]:.3f}]"
            tex.append(rf"Deslocamento (m) & {resultado.desl_media:.3f} & {ic_desl} \\")
            tex.append(r"\bottomrule")
            tex.append(r"\end{tabular}")
            tex.append(r"\end{table}")
            
            if figuras:
                tex.append(r"\subsection{Análise Gráfica}")
                figura_idx = 2
                for nome_fig, fig in figuras:
                    safe_name = f"fig_{figura_idx}.png"
                    prepare_and_save_figure(fig, safe_name)
                    
                    if "tom_de_cinza" in nome_fig:
                        tex.append(r"Para análise espectral transversal ao plano focal, processou-se o perfil de intensidade luminosa ao longo da trajetória de regressão. O sinal analítico contínuo (linha sólida) foi delineado submetendo os dados brutos a uma rotina de suavização matemática por \textit{Spline} não paramétrico bidimensional. Este procedimento visa a mitigação das oscilações de alta frequência inerentes à matriz sensora digital, propiciando a identificação estrutural dos marcadores fiduciários sem comprometer o referencial espacial subjacente (DE BOOR, 1978).")
                    elif "densidade_pontos" in nome_fig:
                        tex.append(r"A dispersão dos pontos simulados via Método de Monte Carlo reflete a propagação das incertezas posicionais. A concentração de densidade em torno da trajetória central permite validar a convergência do modelo estocástico e a estabilidade da solução numérica frente às variações paramétricas das variáveis de entrada.")
                    elif "velocidade_estimada" in nome_fig:
                        tex.append(r"O histograma de frequências da velocidade estimada apresenta a distribuição probabilística da grandeza final. A curva de densidade sobreposta indica a forma da distribuição, possibilitando a extração dos percentis e da estimativa central, fundamentando o intervalo de confiança reportado para a velocidade do veículo.")
                    elif "deslocamento_estimado" in nome_fig:
                        tex.append(r"A distribuição do deslocamento espacial foi obtida através do processo iterativo de simulação. Este gráfico permite avaliar a precisão da medida de distância percorrida, consolidando a base de dados para o cálculo cinemático e a estimativa das incertezas associadas ao referencial métrico.")
                        
                    titulo_bonito = nome_fig.replace("1_", "").replace("2_", "").replace("3_", "").replace("4_", "").replace(".png", "").replace("_", " ").title()
                    tex.append(r"\begin{figure}[H]")
                    tex.append(r"\centering")
                    tex.append(rf"\includegraphics[width=1.0\textwidth,height=0.35\textheight,keepaspectratio]{{{safe_name}}}")
                    tex.append(rf"\caption{{Representação referente a(o) {sanitize(titulo_bonito)}.}}")
                    tex.append(r"\end{figure}")
                    figura_idx += 1
        else:
            tex.append(r"O processamento computacional não conferiu resultados passíveis de exibição.")
            
        tex.append(r"\clearpage")
        tex.append(r"\section{Dados Coletados na Referência}")
        if dados_pontos:
            tex.append(r"\begin{longtable}{cccc}")
            tex.append(r"\caption{Amostragem empírica de coordenadas.} \\")
            tex.append(r"\toprule")
            tex.append(r"\textbf{Grupamento} & \textbf{X (px)} & \textbf{Y (px)} & \textbf{Intensidade} \\")
            tex.append(r"\midrule")
            tex.append(r"\endfirsthead")
            tex.append(r"\toprule")
            tex.append(r"\textbf{Grupamento} & \textbf{X (px)} & \textbf{Y (px)} & \textbf{Intensidade} \\")
            tex.append(r"\midrule")
            tex.append(r"\endhead")
            for p in dados_pontos:
                tex.append(rf"{sanitize(str(p['ponto']))} & {sanitize(str(p['x']))} & {sanitize(str(p['y']))} & {p['cinza']:.4f} \\")
            tex.append(r"\bottomrule")
            tex.append(r"\end{longtable}")
        else:
            tex.append(r"Ausência de dados de entrada anotados.")
            
        if audit_log:
            tex.append(r"\clearpage")
            tex.append(r"\section{Trilha de Auditoria}")
            tex.append(r"Abaixo encontra-se o registro cronológico das ações computacionais e interativas efetuadas no sistema:")
            tex.append(r"\begin{verbatim}")
            for log in audit_log:
                for wl in wrap_verbatim(log):
                    tex.append(wl)
            tex.append(r"\end{verbatim}")
            
        # Adicionar seção de Referências
        tex.append(r"\clearpage")
        tex.append(r"\section{Referências Bibliográficas}")
        tex.append(r"\begin{itemize}")
        tex.append(r"\item BIPM, IEC, IFCC, ILAC, ISO, IUPAC, IUPAP, OIML. \textit{Evaluation of measurement data — Guide to the expression of uncertainty in measurement} (GUM 1995 with minor corrections). JCGM 100:2008. Sèvres, França, 2008.")
        tex.append(r"\item DE BOOR, C. \textit{A Practical Guide to Splines}. 1. ed. New York: Springer-Verlag, 1978.")
        tex.append(r"\item DE MUSIS, C. R.; DE MUSIS, I.; MARTINIS, B.; GROSS, T. J. A Aplicação do Método de Monte Carlo e Razão Cruzada Complexa na Reconstrução de Acidentes de Tráfego. \textit{Revista Brasileira de Criminalística}, v. 14, n. 3, p. 81-89, 2025.")
        tex.append(r"\item DEMING, W. E. \textit{Statistical adjustment of data}. New York: Wiley, 1943.")
        tex.append(r"\item LINNET, K. Evaluation of regression procedures for method comparison studies. \textit{Clinical Chemistry}, v. 39, n. 3, p. 424-432, 1993.")
        tex.append(r"\item MARDIA, K. V. Measures of multivariate skewness and kurtosis with applications. \textit{Biometrika}, v. 57, n. 3, p. 519-530, 1970.")
        tex.append(r"\item MARDIA, K. V. Applications of some measures of multivariate skewness and kurtosis in testing normality and robustness studies. \textit{Sankhyā: The Indian Journal of Statistics, Series B}, v. 36, n. 2, p. 115-128, 1974.")
        tex.append(r"\item SHAPIRO, S. S.; WILK, M. B. An analysis of variance test for normality (complete samples). \textit{Biometrika}, v. 52, n. 3/4, p. 591-611, 1965.")
        tex.append(r"\end{itemize}")

        tex.append(r"\end{document}")
        
        with open(tex_path, "w", encoding="utf-8") as f:
            f.write("\n".join(tex))
            
        # Compilação
        try:
            # Roda duas vezes para arrumar referências
            subprocess.run(["pdflatex", "-interaction=nonstopmode", "report.tex"], cwd=tmp_path, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            subprocess.run(["pdflatex", "-interaction=nonstopmode", "report.tex"], cwd=tmp_path, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            
            # Copia para o destino
            output_pdf = tmp_path / "report.pdf"
            if output_pdf.exists():
                shutil.copy2(output_pdf, caminho_saida)
            else:
                raise RuntimeError("PDF file was not generated by pdflatex.")
        except FileNotFoundError:
            raise RuntimeError("O comando 'pdflatex' não foi encontrado. Verifique se o MiKTeX ou TeX Live está instalado e no PATH.")
        except subprocess.CalledProcessError:
            # Se der erro, tenta ler o log
            log_path = tmp_path / "report.log"
            err_msg = "Erro ao compilar o LaTeX.\n"
            if log_path.exists():
                with open(log_path, "r", encoding="utf-8", errors="ignore") as lf:
                    lines = lf.readlines()
                    errors = [l.strip() for l in lines if l.startswith("!")]
                    if errors:
                        err_msg += "\n".join(errors[:5])
            raise RuntimeError(err_msg)
