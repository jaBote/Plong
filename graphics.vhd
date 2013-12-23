library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity graphics is
    port(
        clk, not_reset: in  std_logic;
		ctl_left, ctl_right: in  std_logic;
        px_x, px_y: in  std_logic_vector(9 downto 0);
        video_on: in  std_logic;
        rgb_stream: out std_logic_vector(2  downto 0);
        ball_bounced: out std_logic;
        ball_missed: out std_logic
    );
end graphics;

architecture dispatcher of graphics is
    constant SCREEN_WIDTH: integer := 640;
    constant SCREEN_HEIGHT: integer := 480;

    type game_states is (start, waiting, playing, game_over);
    signal state, state_next: game_states;

    -- counts how many times the ball hits the bar
    -- used for nothing in particular
    signal bounce_counter, bounce_counter_next: std_logic_vector(7 downto 0);

    signal score_1, score_1_next: std_logic_vector(5 downto 0);
    signal cur_lives, cur_lives_next: std_logic_vector(5 downto 0);
	constant MAX_LIVES: integer := 3;

    signal score_on: std_logic;
    signal current_score: std_logic_vector(5 downto 0);
    signal score_font_addr: std_logic_vector(8 downto 0);

    -- message format is "PLAYER p WINS!"
    -- where p is replaced by player_id
    signal message_on, player_id_on: std_logic;
    signal message_font_addr, player_id_font_addr: std_logic_vector(8 downto 0);

    signal font_addr: std_logic_vector(8 downto 0);
    signal font_data: std_logic_vector(0 to 7);
    signal font_pixel: std_logic;
    signal font_rgb: std_logic_vector(2 downto 0);

    constant BALL_SIZE: integer := 16; -- ball is square
    signal ball_enable: std_logic;
    signal ball_addr: std_logic_vector(3 downto 0);
    signal ball_px_addr: std_logic_vector(3 downto 0);
    signal ball_data: std_logic_vector(0 to BALL_SIZE - 1);
    signal ball_pixel: std_logic;
    signal ball_rgb: std_logic_vector(2 downto 0);
    signal ball_x, ball_x_next: std_logic_vector(9 downto 0);
    signal ball_y, ball_y_next: std_logic_vector(9 downto 0);
	signal ball_dx, ball_dy: integer;

    signal ball_h_dir, ball_h_dir_next, ball_v_dir, ball_v_dir_next: std_logic;

    signal ball_bounce, ball_miss: std_logic;

    constant BAR_1_POS: integer := 20; -- This is now the vertical height the bar starts in

    constant BAR_WIDTH: integer := 128;
    constant BAR_HEIGHT: integer := 20;

    signal bar_pos: integer;
    signal bar_addr: std_logic_vector(5 downto 0);
    signal bar_data: std_logic_vector(0 to BAR_WIDTH - 1);
    signal bar_pixel: std_logic;
    signal bar_rgb: std_logic_vector(2 downto 0);
    signal bar_1_x, bar_1_x_next: std_logic_vector(9 downto 0);

    signal ball_on, bar_on: std_logic;
begin

    process(state, ball_y, ctl_left, ctl_right, cur_lives)
    begin
        state_next <= state;
        ball_enable <= '0';
        ball_miss <= '0';
        cur_lives_next <= cur_lives;

        case state is
            when start =>
                cur_lives_next <= conv_std_logic_vector(MAX_LIVES,6);
                state_next <= waiting;
            when waiting =>
                ball_enable <= '0';
                if cur_lives = 0  then
                    state_next <= game_over;
                elsif (ctl_left = '1' OR ctl_right = '1') then
                    state_next <= playing;
                end if;
            when playing =>
                ball_enable <= '1';
                if ball_y = 0 then
                    -- player 1 loses a live. Y = 0 is the bottom of the screen
                    cur_lives_next <= cur_lives - 1;
                    state_next <= waiting;
                    ball_miss <= '1';
                end if;
            when game_over =>
                if (ctl_left = '1' OR ctl_right = '1') then
                    state_next <= start;
                end if;
        end case;
    end process;

    process(clk, not_reset)
    begin
        if not_reset = '0' then
            state <= start;
            ball_x <= (others => '0');
            ball_y <= (others => '0');
            bar_1_x <= conv_std_logic_vector(SCREEN_WIDTH / 2 - BAR_WIDTH / 2, 10);
            ball_h_dir <= '0';
            ball_v_dir <= '0';
            bounce_counter <= (others => '0');
            score_1 <= (others => '0');
            cur_lives <= (others => '0');
        elsif clk'event and clk = '0' then
            state <= state_next;
            ball_x <= ball_x_next;
            ball_y <= ball_y_next;
            bar_1_x <= bar_1_x_next;
            ball_h_dir <= ball_h_dir_next;
            ball_v_dir <= ball_v_dir_next;
            bounce_counter <= bounce_counter_next;
            score_1 <= score_1_next;
            cur_lives <= cur_lives_next;
        end if;
    end process;

    score_on <= '1' when px_y(9 downto 3) = 1 and
                         (px_x(9 downto 3) = 42 or px_x(9 downto 3) = 37) else
                '0';
    -- We used the previous score_2 signal to store lives
	current_score <= score_1 when px_x < 320 else cur_lives;
    -- numbers start at memory location 128
    -- '1' starts at 136, '2' at 144 and so on
    score_font_addr <= conv_std_logic_vector(128, 9) +
                       (current_score(2 downto 0) & current_score(5 downto 3));

    player_id_on <= '1' when state = game_over and px_y(9 downto 3) = 29 and
                             (px_x(9 downto 3) = 19 or px_x(9 downto 3) = 59) else
                    '0';
    -- player_id will display either 1 or 2
    player_id_font_addr <= "010001000" when px_x < 320 else "010010000";

    message_on <= '1' when state = game_over and
                           -- message on the center of the screen
                           ( px_x(9 downto 3) >= 32 and
                             px_x(9 downto 3) < 46 and
                             px_y(9 downto 3) = 29 ) else
                  '0';
    
	-- TODO change into GAME OVER
	with px_x(9 downto 3) select
        message_font_addr <= "110000000" when "0110100"|"0001100", -- P
                             "101100000" when "0110101"|"0001101", -- L
                             "100001000" when "0110110"|"0001110", -- A
                             "111001000" when "0110111"|"0001111", -- Y
                             "100101000" when "0111000"|"0010000", -- E
                             "110010000" when "0111001"|"0010001", -- R
                             "111100000" when "0111011"|"0010011", -- not visible
                             "110111000" when "0111101"|"0010101", -- W
                             "101111000" when "0111110"|"0010110", -- O
                             "101110000" when "0111111"|"0010111", -- N
                             "000001000" when "1000000"|"0011000", -- !
                             "000000000" when others;

    -- font address mutltiplexer
    font_addr <= px_y(2 downto 0) + score_font_addr when score_on = '1' else
                 px_y(2 downto 0) + player_id_font_addr when player_id_on = '1' else
                 px_y(2 downto 0) + message_font_addr when message_on = '1' else
                 (others => '0');
    font_pixel <= font_data(conv_integer(px_x(2 downto 0)));
    font_rgb <= "000" when font_pixel = '1' else "111";

    direction_control: process(
        ball_x, ball_y,
        ball_h_dir, ball_v_dir,
        ball_h_dir_next, ball_v_dir_next,
        bar_1_x,
        not_reset
    )
    begin
        ball_h_dir_next <= ball_h_dir;
        ball_v_dir_next <= ball_v_dir;
        ball_bounce <= '0';

		-- Changed most of this process code since now our bar is horizontal
		
        --
        -- BEWARE! Looks like ball_bounce signal is generated twice
        -- due to slower clock! Too lazy to fix now :D
        --
      if not_reset = '0' then
			-- This way, when enabled, ball will start descending to right at 45 degrees
			ball_dx <= 1;
			ball_dy <= 1;
	   elsif  ball_y = BAR_1_POS + BAR_HEIGHT and
            ball_x + BALL_SIZE > bar_1_x and
            ball_x < bar_1_x + BAR_WIDTH then
                ball_v_dir_next <= '1';
                ball_bounce <= '1';
				-- Now that we know ball bounces we can make some gross estimations...
				-- It's also grossly unadvised for performance's sake, but I won't pre-make any divisions for clarity's sake
				-- First 10% is rebound 30 degrees to left.
				if ((conv_integer(ball_x) + BALL_SIZE) / 2) < (conv_integer(bar_1_x) + (BAR_WIDTH * 10 ) / 100) then
					ball_h_dir_next <= '0';
					ball_dx <= 2;
					ball_dy <= 1;
				-- Then 15% for rebound 45 degrees to left.
				elsif ((conv_integer(ball_x) + BALL_SIZE) / 2) < (conv_integer(bar_1_x) + (BAR_WIDTH * 25) / 100) then
					ball_h_dir_next <= '0';
					ball_dx <= 1;
					ball_dy <= 1;
				-- Then 25% for rebound 60 degrees to left.
				elsif ((conv_integer(ball_x) + BALL_SIZE) / 2) < (conv_integer(bar_1_x) + (BAR_WIDTH * 50) / 100) then
					ball_h_dir_next <= '0';
					ball_dx <= 1;
					ball_dy <= 2;
				-- Then vertical rebound if you're lucky enough. dy is set to 2 for preventing the ball to appear to be moving too slow with this
				elsif ((conv_integer(ball_x) + BALL_SIZE) / 2) = (conv_integer(bar_1_x) + (BAR_WIDTH * 50) / 100) then
					ball_dx <= 0;
					ball_dy <= 2;
				-- Then 25% for rebound 60 degrees to right.
				elsif ((conv_integer(ball_x) + BALL_SIZE) / 2) < (conv_integer(bar_1_x) + (BAR_WIDTH * 75) / 100) then
					ball_h_dir_next <= '1';
					ball_dx <= 1;
					ball_dy <= 2;
				-- Then 15% for rebound 60 degrees to right.
				elsif ((conv_integer(ball_x) + BALL_SIZE) / 2) < (conv_integer(bar_1_x) + (BAR_WIDTH * 90) / 100) then
					ball_h_dir_next <= '1';
					ball_dx <= 1;
					ball_dy <= 1;
				-- Then 10% for rebound 60 degrees to right. Rest of the bar
				else
					ball_h_dir_next <= '1';
					ball_dx <= 2;
					ball_dy <= 1;
				end if;
		-- Ball hits the top of screen
		elsif ball_y = SCREEN_HEIGHT - BALL_SIZE then
            ball_v_dir_next <= '0';
		-- Ball is way too late for being saved but you hit it with any of the sides of the bar
		elsif ball_y < BAR_1_POS + BAR_WIDTH and ball_y + BALL_SIZE > BAR_1_POS then
			if ball_x + BALL_SIZE > BAR_1_POS then
				ball_h_dir_next <= '0';
			elsif ball_x = bar_1_x + BAR_WIDTH then
				ball_h_dir_next <= '1';
			end if;
		end if;
		
		-- Collision with left or right of screen
        if ball_x = 0 then
            ball_v_dir_next <= '1';
        elsif ball_x = SCREEN_WIDTH - BALL_SIZE then
            ball_v_dir_next <= '0';
        end if;
    end process;

    bounce_counter_next <= bounce_counter + 1 when ball_bounce = '1' else
                           (others => '0') when ball_miss = '1' else
                           bounce_counter;

    ball_control: process(
        ball_x, ball_y,
        ball_x_next, ball_y_next,
        ball_h_dir, ball_v_dir,
		ball_dx, ball_dy,
        ball_enable
    )
    begin
        ball_x_next <= ball_x;
        ball_y_next <= ball_y;

        if ball_enable = '1' then
            if ball_h_dir = '1' then
                ball_x_next <= ball_x + conv_std_logic_vector(ball_dx, 2);
            else
                ball_x_next <= ball_x - conv_std_logic_vector(ball_dx, 2);
            end if;
            if ball_v_dir = '1' then
                ball_y_next <= ball_y + conv_std_logic_vector(ball_dy, 2);
            else
                ball_y_next <= ball_y - conv_std_logic_vector(ball_dy, 2);
            end if;
        else
            ball_x_next <= conv_std_logic_vector(SCREEN_WIDTH / 2 - BALL_SIZE / 2, 10);
            ball_y_next <= conv_std_logic_vector(SCREEN_HEIGHT / 2 - BALL_SIZE / 2, 10);
        end if;
    end process;

    bar_control: process(
        bar_1_x,
        ctl_left, ctl_right
    )
    begin
        bar_1_x_next <= bar_1_x;
        
        if ctl_left = '1' then
            if bar_1_x > 0 then
                bar_1_x_next <= bar_1_x - 1;
            end if;
        elsif ctl_right = '1' and bar_1_x + BAR_WIDTH + 1 < SCREEN_HEIGHT then
                bar_1_x_next <= bar_1_x + 1;
        end if;
    end process;

    ball_on <= '1' when px_x >= ball_x and
                        px_x < (ball_x + BALL_SIZE) and
                        px_y >= ball_y and
                        px_y < (ball_y + BALL_SIZE) else
               '0';

    -- whether bar_1 is on
    bar_on <= '1' when (px_y >= BAR_1_POS and
                        px_y < BAR_1_POS + BAR_HEIGHT and
                        px_x >= bar_1_x and
                        px_x < bar_1_x + BAR_WIDTH) else
              '0';

    ball_addr <= px_y(3 downto 0) - ball_y(3 downto 0);
    ball_px_addr <= px_x(3 downto 0) - ball_x(3 downto 0);
    ball_pixel <= ball_data(conv_integer(ball_px_addr));
    ball_rgb <= "000" when ball_pixel = '1' else "111";


    bar_addr <= (px_x(5 downto 0) - bar_1_x(5 downto 0));
    bar_pos <= BAR_1_POS;
    bar_pixel <= bar_data(conv_integer(px_y - bar_pos));
    bar_rgb <= "000" when bar_pixel = '1' else "111";

    process(
        ball_on, bar_on,
        ball_rgb, bar_rgb,
        score_on, message_on, font_rgb,
        video_on
    )
    begin
        if video_on = '1' then
            if bar_on = '1' then
                rgb_stream <= bar_rgb;
            elsif ball_on = '1' then
                rgb_stream <= ball_rgb;
            -- scores and messages share rgb stream
            elsif score_on = '1' or message_on = '1' then
                rgb_stream <= font_rgb;
            else
                -- background is white
                rgb_stream <= "111";
            end if;
        else
            -- blank screen
            rgb_stream <= "000";
        end if;
    end process;

    ball_unit:
        entity work.ball_rom(content)
        port map(addr => ball_addr, data => ball_data);

    bar_unit:
        entity work.bar_rom(content)
        port map(clk => clk, addr => bar_addr, data => bar_data);

    font_unit:
        entity work.codepage_rom(content)
        port map(addr => font_addr, data => font_data);

    ball_bounced <= ball_bounce;
    ball_missed <= ball_miss;

end dispatcher;
