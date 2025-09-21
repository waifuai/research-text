# has many errors
# manim -pqh 14-manim-dynamic-commission.py TokenAffiliatesAnimation
from manim import *
import numpy as np

class TokenAffiliatesAnimation(Scene):
    def construct(self):
        self.intro_scene()
        self.dashboard_scene()
        self.link_commission_scene()
        self.transaction_payout_scene()
        self.benefits_scene()

    def intro_scene(self):
        title = Text("TokenAffiliates", font_size=60).to_edge(UP)
        subtitle = Text("Dynamic Commission System", font_size=40).next_to(title, DOWN)

        # Create a token-like shape using Manim primitives
        token_icon = VGroup(
            Circle(radius=0.5, fill_color=YELLOW, fill_opacity=1),
            Polygon(
                [0.5 * np.cos(angle), 0.5 * np.sin(angle), 0]
                for angle in np.linspace(0, 2 * np.pi, 6, endpoint=False)
            ).scale(0.4).rotate(PI/6).set_color(ORANGE).set_fill(ORANGE, opacity=0.8)
        ).next_to(subtitle, DOWN, buff=1)

        network_dots = [Dot(radius=0.1).shift(RIGHT * i + UP * np.sin(i)) for i in range(-3, 4)]
        network_lines = VGroup()
        for dot in network_dots:
            line = Line(token_icon.get_center(), dot.get_center(), stroke_width=2)
            network_lines.add(line)

        self.play(Write(title), Write(subtitle))
        self.play(Create(token_icon), run_time=1.5)
        self.play(*[Create(dot) for dot in network_dots], run_time=1)
        self.play(*[Create(line) for line in network_lines], run_time=1)

        empowering_text = Text("Empowering Affiliates with Flexible Commissions", font_size=30).next_to(token_icon, DOWN, buff=1)
        self.play(Write(empowering_text))
        self.wait(2)
        self.play(*[FadeOut(mob) for mob in self.mobjects)

    def dashboard_scene(self):
        title = Text("Affiliate Dashboard", font_size=40).to_edge(UP)
        self.play(Write(title))

        dashboard_frame = RoundedRectangle(width=12, height=6, corner_radius=0.3, stroke_width=2).move_to(DOWN*0.5)
        self.play(Create(dashboard_frame))

        sections = ["Token Listing", "Referral Links", "Commission Rates", "Performance", "Payouts"]
        section_rects = [
        for i, section in enumerate(sections):
            rect = RoundedRectangle(width=2, height=1, corner_radius=0.2, fill_color=BLUE, fill_opacity=0.3)
            rect.move_to(dashboard_frame.get_left() + RIGHT*(i*2.4 +1.2) + UP * 1.5 )
            text = Text(section, font_size=15).move_to(rect)
            section_rects.append(VGroup(rect, text))
            self.play(FadeIn(rect), Write(text))
            self.wait(0.2)

        # Focus on Commission Rates
        self.play(section_rects[2].animate.scale(1.2).set_color(YELLOW))
        commission_details = VGroup(
            Text("Default Rate: 10%", font_size=20),
            Text("Customizable Per Token", font_size=20)
        ).arrange(DOWN, aligned_edge=LEFT).next_to(section_rects[2], DOWN)
        self.play(Write(commission_details))
        self.wait(1)

        self.play(FadeOut(title), FadeOut(dashboard_frame), *[FadeOut(sr) for sr in section_rects], FadeOut(commission_details))

    def link_commission_scene(self):
        title = Text("Referral Links & Commissions", font_size=40).to_edge(UP)
        self.play(Write(title))

        token_symbol = Text("TOKEN", font_size=30).to_edge(LEFT).shift(UP)
        token_box = SurroundingRectangle(token_symbol, color=YELLOW, buff=.2)
        self.play(Write(token_symbol), Create(token_box))

        link_label = Text("Referral Link:", font_size=20).next_to(token_box, RIGHT, buff=1).shift(UP*0.5)
        referral_link = Text("https://tokenaffiliates.com/ref?token=xyz789", font_size=18, line_spacing=-0.3).next_to(link_label, DOWN, aligned_edge=LEFT)
        link_box = SurroundingRectangle(VGroup(link_label, referral_link), color=BLUE, buff=0.2)
        self.play(Write(link_label), Write(referral_link), Create(link_box))

        commission_label = Text("Commission Rate:", font_size=20).next_to(link_box, DOWN, buff=1)
        commission_value = DecimalNumber(10, num_decimal_places=0, unit="%", font_size=20).next_to(commission_label, RIGHT)
        commission_slider = Line(LEFT, RIGHT, length=2).next_to(commission_label, DOWN, buff=0.5)
        slider_handle = Triangle(start_angle=PI/2, color=WHITE, fill_color=RED, fill_opacity=1).scale(0.1).move_to(commission_slider.get_center())

        self.play(Write(commission_label), Write(commission_value), Create(commission_slider), Create(slider_handle))

        potential_earnings_label = Text("Potential Earnings:", font_size=20).next_to(commission_slider, DOWN)
        potential_earnings_value = DecimalNumber(0, num_decimal_places=2, unit="$", font_size=20).next_to(potential_earnings_label, RIGHT)
        self.play(Write(potential_earnings_label), Write(potential_earnings_value))

        def update_earnings(mob):
            rate = commission_value.get_value() / 100
            earnings = 1000 * rate
            potential_earnings_value.set_value(earnings)

        self.play(slider_handle.animate.shift(RIGHT),
                  commission_value.animate.set_value(15),
                  UpdateFromFunc(potential_earnings_value, update_earnings),
                  run_time=1)
        self.wait(1)
        self.play(FadeOut(title), FadeOut(token_symbol), FadeOut(token_box),  FadeOut(link_label),  FadeOut(referral_link), FadeOut(link_box), FadeOut(commission_label),  FadeOut(commission_value), FadeOut(commission_slider), FadeOut(slider_handle), FadeOut(potential_earnings_label), FadeOut(potential_earnings_value))

    def transaction_payout_scene(self):
        title = Text("Transaction & Payout", font_size=40).to_edge(UP)
        self.play(Write(title))

        user_icon = VGroup(Circle(radius=0.4, color=BLUE), Dot(radius=0.1).shift(UP*0.2), Line(LEFT*0.2, RIGHT*0.2, stroke_width=2).shift(DOWN*0.2)).to_edge(LEFT)
        referral_link_box = Rectangle(width=3, height=0.8, color=BLUE, fill_opacity=0.3).next_to(user_icon, RIGHT)
        link_text = Text("Referral Link", font_size=15).move_to(referral_link_box)

        blockchain_icon = Square(side=0.8, color=GREEN).to_edge(RIGHT)
        chain_links = VGroup(*[Circle(radius=0.05, stroke_width=2).shift(RIGHT*0.2*i) for i in range(-2,3)]).move_to(blockchain_icon)
        blockchain_icon = VGroup(blockchain_icon, chain_links)
        transaction_line = Arrow(referral_link_box.get_right(), blockchain_icon.get_left(), buff=0.5)

        self.play(FadeIn(user_icon), Create(referral_link_box), Write(link_text))
        self.play(user_icon.animate.shift(RIGHT * 2), run_time=0.5)
        self.play(Create(transaction_line))
        self.play(FadeIn(blockchain_icon))

        investment_amount = 1000
        commission_rate = 0.15
        commission_amount = investment_amount * commission_rate

        investment_text = Text(f"Investment: ${investment_amount}", font_size=20).next_to(blockchain_icon, DOWN)
        commission_text = Text(f"Commission: 15%", font_size=20).next_to(investment_text, DOWN)
        calculated_commission = Text(f"Calculated: ${commission_amount}", font_size=20).next_to(commission_text, DOWN)

        self.play(Write(investment_text), Write(commission_text))
        self.play(Write(calculated_commission))

        wallet_icon = VGroup(Rectangle(width=0.8, height=0.6, color=YELLOW), Ellipse(width=0.6, height=0.3, color=YELLOW).shift(UP*0.4)).next_to(blockchain_icon, DOWN, buff=2)
        payment_line = Arrow(blockchain_icon.get_bottom(), wallet_icon.get_top(), buff=0.5)
        self.play(Create(payment_line))
        self.play(FadeIn(wallet_icon))

        payment_text = Text(f"+ ${commission_amount}", font_size=20).next_to(wallet_icon, DOWN)
        self.play(Write(payment_text))

        self.wait(2)

        self.play(FadeOut(title), FadeOut(user_icon), FadeOut(referral_link_box), FadeOut(link_text), FadeOut(blockchain_icon), FadeOut(investment_text), FadeOut(commission_text), FadeOut(calculated_commission), FadeOut(wallet_icon), FadeOut(payment_text), FadeOut(payment_line), FadeOut(transaction_line))

    def benefits_scene(self):
        title = Text("Benefits of Dynamic Commissions", font_size=40).to_edge(UP)
        self.play(Write(title))

        benefits = [
            ("Flexibility", "Affiliates control their earnings."),
            ("Incentivization", "Higher rates attract promotion."),
            ("Adaptability", "Respond to market changes quickly."),
            ("Competition", "Stand out from fixed-rate programs.")
        ]

        benefit_mobs = VGroup()
        for i, (title_text, desc_text) in enumerate(benefits):
            title_mob = Text(title_text, font_size=25)
            desc_mob = Text(desc_text, font_size=18)
            benefit_group = VGroup(title_mob, desc_mob).arrange(DOWN, aligned_edge=LEFT)
            benefit_group.shift(DOWN * i * 1.2)
            benefit_mobs.add(benefit_group)

        for benefit_group in benefit_mobs:
            self.play(Write(benefit_group), run_time=0.8)
            self.wait(0.5)

        self.wait(1)

        conclusion_text = Text("TokenAffiliates: Empowering Affiliate Success.", font_size=30).to_edge(DOWN)
        self.play(Write(conclusion_text))
        self.wait(2)