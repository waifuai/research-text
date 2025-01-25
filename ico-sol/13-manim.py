from manim import *

class TokenizedEconomyIntro(Scene):
    def construct(self):
        # Title
        title = Text("The Tokenized Economy", font_size=48)
        subtitle = Text("A Solana-Based Barter System", font_size=24).next_to(title, DOWN)
        solana_logo = VGroup(Circle(radius=0.2, color=TEAL), Square(side_length=0.3, color=PURPLE).rotate(PI/4)).scale(0.8).next_to(subtitle, DOWN)
        self.play(Write(title), Write(subtitle), Create(solana_logo))
        self.wait(1)
        self.play(FadeOut(title), FadeOut(subtitle), FadeOut(solana_logo))

        # Problem Statement
        problems = [Text(f"Problem: {p}", font_size=30) for p in ["Capital Barriers", "Speculative Markets", "Short-Term Focus"]]
        for problem in problems:
            self.play(Write(problem))
            self.wait(0.8)
            self.play(FadeOut(problem))

        # Solution
        solution = Text("Solution: Tokenized Bartering", font_size=36)
        arrows = VGroup(Arrow(LEFT, RIGHT), Arrow(RIGHT, LEFT)).scale(0.5).next_to(solution, DOWN)
        self.play(Write(solution), Create(arrows))
        self.wait(1)
        self.play(FadeOut(solution), FadeOut(arrows))

        # Core Concept
        company = Rectangle(width=1, height=0.5, color=BLUE, fill_opacity=0.5)
        company_text = Text("Company", font_size=16).move_to(company)
        token = Rectangle(width=0.3, height=0.3, color=YELLOW, fill_opacity=1).next_to(company, RIGHT)
        token_text = Text("Token", font_size=14).move_to(token)
        ico_text = Text("Companies issue Utility Tokens via ICOs (on Solana)", font_size=20).to_edge(UP)
        use_text = Text("Tokens are used to buy Products & Services", font_size=20).next_to(ico_text, DOWN)
        self.play(Write(ico_text), Create(company), Write(company_text), Create(token), Write(token_text), Write(use_text))
        self.wait(1)
        self.play(FadeOut(company), FadeOut(company_text), FadeOut(token), FadeOut(token_text), FadeOut(ico_text), FadeOut(use_text))

class TokenAffiliatesMechanism(Scene):
    def construct(self):
        # Affiliate and ICO
        affiliate_text = Text("Affiliate", font_size=24).to_edge(LEFT)
        ico_text = Text("ICO", font_size=24).to_edge(RIGHT)
        self.play(Write(affiliate_text), Write(ico_text))
        self.wait(0.5)

        # Link
        link = Line(affiliate_text.get_right(), ico_text.get_left(), buff=0.5).set_color(BLUE)
        link_text = Text("Referral Link", font_size=16).next_to(link, UP, buff=0.2)
        self.play(Create(link), Write(link_text))
        self.wait(0.5)

        # Investor, Investment, Commission
        investor_text = Text("Investor", font_size=20).next_to(link, DOWN)
        invest_text = Text("Investor invests through Link", font_size=18).next_to(investor_text, DOWN)
        self.play(Write(investor_text), Write(invest_text))

        commission_text = Text("Affiliate earns 10% Commission", font_size=18).next_to(invest_text, DOWN)
        bar_chart = VGroup(
            Rectangle(width=1, height=0.5, color=GREEN, fill_opacity=1),
            Rectangle(width=0.1, height=0.5, color=YELLOW, fill_opacity=1).next_to(Rectangle(width=1, height=0.5, color=GREEN, fill_opacity=1), RIGHT, buff=0)
        ).scale(0.4).next_to(commission_text, DOWN)
        self.play(Write(commission_text), Create(bar_chart))
        self.wait(1)
        self.play(FadeOut(investor_text), FadeOut(invest_text), FadeOut(commission_text), FadeOut(bar_chart), FadeOut(affiliate_text), FadeOut(ico_text), FadeOut(link), FadeOut(link_text))

        # Risk-Free
        risk_free_text = Text("No Upfront Investment Required!", font_size=24)
        dollar_sign = Text("$", font_size=30)
        cross = Line(dollar_sign.get_corner(UL), dollar_sign.get_corner(DR), color=RED)
        self.play(Write(risk_free_text))
        self.wait(0.5)
        self.play(Write(dollar_sign))
        self.wait(0.3)
        self.play(Create(cross))
        self.wait(1)
        self.play(FadeOut(risk_free_text), FadeOut(dollar_sign), FadeOut(cross))

class ConcludingScene(Scene):
    def construct(self):
        # Summary
        summary_texts = [
            Text("Tokenized Economy", font_size=30),
            Text("TokenAffiliates", font_size=30),
            Text("Decentralized Growth", font_size=30),
            Text("Sustainable Future", font_size=30),
        ]
        for i, text in enumerate(summary_texts):
            text.shift(UP * (1.5 - 0.5 * i))  # Arrange vertically
            self.play(Write(text))
            self.wait(0.5)

        self.wait(1)
        self.play(*[FadeOut(text) for text in summary_texts])

        # Final Scene
        solana_logo = VGroup(Circle(radius=0.2, color=TEAL), Square(side_length=0.3, color=PURPLE).rotate(PI/4)).scale(0.8)
        final_text = Text("Building the Future of Commerce", font_size=36).next_to(solana_logo, DOWN)
        self.play(Create(solana_logo), Write(final_text))
        self.wait(2)